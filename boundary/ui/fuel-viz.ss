;;; boundary/fuel-viz.ss — Fuel Visualization for Debugger
;;;
;;; Rich ASCII visualizations of fuel consumption:
;;;   - Fuel timeline (consumption over steps)
;;;   - Fuel heatmap (expression costs)
;;;   - Fuel flamegraph
;;;   - Fuel summary with bars
;;;
;;; This is Shell code: display formatting.
;;;
;;; Dependencies:
;;;   - core/util/debug.ss
;;;   - boundary/debug/session-debugger.ss

(load "core/util/debug.ss")
(load "boundary/debug/session-debugger.ss")

;;; ====
;;; Configuration
;;; ====

(define *viz-width* 60)
(define *viz-height* 10)

;;; ====
;;; Fuel Timeline
;;; ====

;;; fuel-timeline : Debugger → String
;;; Show fuel consumption over time as ASCII graph.
(define (fuel-timeline dbg)
  (let* ([fuel-trace (debugger-fuel-trace dbg)]
         [budget (debugger-fuel-budget dbg)]
         [entries (reverse fuel-trace)]
         [n-entries (length entries)]
         [width (min *viz-width* n-entries)]
         [height *viz-height*]
         [lines (make-vector height "")]
         [scale (if (zero? budget) 1 (/ height budget))])
        
        (if (null? entries)
            "  (no data)\n"
            
            (begin
             ;; Build each column
             (let loop-cols ([i 0] [remaining entries])
                  (when (and (< i width) (pair? remaining))
                        (let* ([entry (car remaining)]
                               [used (cadddr entry)]
                               [bar-height (min height (max 1 (round (* used scale 10))))])
                              ;; Fill column
                              (let loop-rows ([j 0])
                                   (when (< j height)
                                         (let ([row (- height j 1)])
                                              (vector-set! lines row
                                                           (string-append
                                                            (vector-ref lines row)
                                                            (if (< j bar-height) "#" " "))))
                                         (loop-rows (+ j 1))))
                              (loop-cols (+ i 1) (cdr remaining)))))
             
             ;; Render
             (string-append
              "\n  FUEL TIMELINE (consumption per step)\n"
              "  " (make-string *viz-width* #\-) "\n"
              (let loop ([i 0] [acc ""])
                   (if (>= i height)
                       acc
                       (loop (+ i 1)
                             (string-append acc "  |" (vector-ref lines i) "\n"))))
              "  +" (make-string *viz-width* #\-) "+\n"
              "   Step 1" (make-string (- *viz-width* 10) #\space)
              (format "Step ~a\n" n-entries))))))

;;; ====
;;; Fuel Heatmap
;;; ====

;;; fuel-heatmap : Debugger → String
;;; Show expressions colored by fuel cost.
(define (fuel-heatmap dbg)
  (let* ([fuel-trace (debugger-fuel-trace dbg)]
         [budget (debugger-fuel-budget dbg)]
         [hotspots (fuel-hotspots dbg 10)])
        
        (string-append
         "\n  FUEL HEATMAP (top consumers)\n"
         "  " (make-string 50 #\-) "\n\n"
         (if (null? hotspots)
             "  (no data)\n"
             (apply string-append
                    (map (lambda (entry)
                                 (let* ([expr (car entry)]
                                        [fuel-before (cadr entry)]
                                        [fuel-after (caddr entry)]
                                        [used (cadddr entry)]
                                        [pct (if (zero? budget) 0
                                                 (* 100.0 (/ used budget)))]
                                        [bar-len (min 30 (max 1 (round (* 30 (/ pct 100)))))]
                                        [bar (make-string bar-len #\#)])
                                       (format "  ~a ~a ~a (~a fuel)\n"
                                               (truncate-viz-expr expr 20)
                                               bar
                                               (round pct)
                                               used)))
                         hotspots)))
         "\n")))

;;; truncate-viz-expr : Expr × Nat → String
(define (truncate-viz-expr expr max-len)
  (let* ([str (format "~a" expr)]
         [len (string-length str)])
        (if (> len max-len)
            (string-append (substring str 0 (- max-len 3)) "...")
            (string-append str (make-string (- max-len len) #\space)))))

;;; ====
;;; Fuel Breakdown
;;; ====

;;; fuel-breakdown : Debugger → String
;;; Show fuel breakdown by expression type.
(define (fuel-breakdown dbg)
  (let* ([by-type (fuel-by-expr-type dbg)]
         [total (debugger-fuel-used dbg)]
         [sorted (list-sort (lambda (a b) (> (cdr a) (cdr b))) by-type)])
        
        (string-append
         "\n  FUEL BY EXPRESSION TYPE\n"
         "  " (make-string 50 #\-) "\n\n"
         (if (null? sorted)
             "  (no data)\n"
             (apply string-append
                    (map (lambda (entry)
                                 (let* ([type (car entry)]
                                        [used (cdr entry)]
                                        [pct (if (zero? total) 0
                                                 (* 100.0 (/ used total)))]
                                        [bar-len (min 25 (max 1 (round (* 25 (/ pct 100)))))]
                                        [bar (make-string bar-len #\#)]
                                        [empty (make-string (- 25 bar-len) #\-)])
                                       (format "  ~a |~a~a| ~a% (~a)\n"
                                               (pad-type type 12)
                                               bar empty
                                               (round pct)
                                               used)))
                         sorted)))
         "\n")))

;;; pad-type : Symbol × Nat → String
(define (pad-type sym len)
  (let* ([str (symbol->string sym)]
         [slen (string-length str)])
        (if (>= slen len)
            (substring str 0 len)
            (string-append str (make-string (- len slen) #\space)))))

;;; ====
;;; Full Fuel Report
;;; ====

;;; fuel-report : Debugger → String
;;; Generate complete fuel consumption report.
(define (fuel-report dbg)
  (let* ([budget (debugger-fuel-budget dbg)]
         [used (debugger-fuel-used dbg)]
         [remaining (debugger-fuel dbg)]
         [pct (debugger-fuel-pct dbg)]
         [steps (length (debugger-trace dbg))]
         [status (debugger-status dbg)])
        
        (string-append
         "\n"
         "  ====\n"
         "                      FUEL REPORT\n"
         "  ====\n\n"
         
         (format "  Status:      ~a\n" status)
         (format "  Budget:      ~a fuel\n" budget)
         (format "  Used:        ~a fuel (~a%)\n" used (round pct))
         (format "  Remaining:   ~a fuel\n" remaining)
         (format "  Steps:       ~a\n\n" steps)
         
         ;; Progress bar
         (let* ([bar-width 50]
                [filled (round (* bar-width (/ pct 100)))]
                [empty (- bar-width filled)])
               (format "  [~a~a]\n\n"
                       (make-string filled #\#)
                       (make-string empty #\-)))
         
         ;; Breakdown
         (fuel-breakdown dbg)
         
         ;; Heatmap
         (fuel-heatmap dbg))))

;;; ====
;;; Display Functions
;;; ====

;;; display-fuel-timeline : Debugger → void
(define (display-fuel-timeline dbg)
  (display (fuel-timeline dbg)))

;;; display-fuel-heatmap : Debugger → void
(define (display-fuel-heatmap dbg)
  (display (fuel-heatmap dbg)))

;;; display-fuel-breakdown : Debugger → void
(define (display-fuel-breakdown dbg)
  (display (fuel-breakdown dbg)))

;;; display-fuel-report : Debugger → void
(define (display-fuel-report dbg)
  (display (fuel-report dbg)))

;;; ====
;;; REPL Commands
;;; ====

;;; cmd-fuel-report : → void
(define (cmd-fuel-report)
  (let ([dbg (require-debugger!)])
       (display-fuel-report dbg)
       (void)))

;;; cmd-fuel-timeline : → void
(define (cmd-fuel-timeline)
  (let ([dbg (require-debugger!)])
       (display-fuel-timeline dbg)
       (void)))

;;; cmd-fuel-heatmap : → void
(define (cmd-fuel-heatmap)
  (let ([dbg (require-debugger!)])
       (display-fuel-heatmap dbg)
       (void)))
