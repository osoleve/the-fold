;;; boundary/ui/profile-viz.ss — Profile Visualization
;;;
;;; ASCII-art visualizations for profiler output:
;;;   - Flame graphs
;;;   - Call trees with fuel bars
;;;   - Fuel budget pie charts
;;;
;;; This is Shell code: handles display formatting.
;;;
;;; Dependencies:
;;;   - core/util/profile.ss

(load "core/util/profile.ss")

;;; ====
;;; Flame Graph Rendering
;;; ====

;;; A flame graph shows call stacks with width proportional to fuel.
;;; Stack frames are shown bottom-up (root at bottom).

(define *flame-width* 60)  ; Total character width
(define *flame-chars* '(#\space #\. #\: #\| #\# #\@))

;;; render-flame-graph : Profiler → String
;;; Render an ASCII flame graph
(define (render-flame-graph p)
  (let* ([root (profiler-root p)]
         [total-fuel (node-fuel-consumed root)]
         [lines (flame-lines root 0 *flame-width* total-fuel)])
        (string-append
         "\n  FLAME GRAPH (width = fuel consumption)\n"
         "  " (make-string *flame-width* #\=) "\n"
         (apply string-append lines)
         "  " (make-string *flame-width* #\=) "\n")))

;;; flame-lines : Node × Depth × Width × TotalFuel → List of Strings
;;; Generate lines for a node and its children
(define (flame-lines node depth width total-fuel)
  (let* ([fuel (node-fuel-consumed node)]
         [proportion (if (zero? total-fuel) 0 (/ fuel total-fuel))]
         [bar-width (max 1 (round (* width proportion)))]
         [name (node-name node)]
         [name-str (symbol->string name)]
         [truncated (if (> (string-length name-str) (- bar-width 2))
                        (string-append (substring name-str 0 (max 1 (- bar-width 5))) "...")
                        name-str)]
         [padding-left (quotient (- bar-width (string-length truncated)) 2)]
         [bar (string-append
               (make-string padding-left #\space)
               truncated
               (make-string (- bar-width padding-left (string-length truncated)) #\space))]
         [frame (format "  ~a[~a]\n"
                        (make-string (* depth 2) #\space)
                        bar)]
         [children (node-children node)]
         [child-lines (apply append
                             (map (lambda (c)
                                          (flame-lines c (+ depth 1) bar-width fuel))
                                  (reverse children)))])
        (cons frame child-lines)))

;;; ====
;;; Horizontal Bar Chart
;;; ====

;;; render-fuel-bars : Profiler × Nat → String
;;; Render horizontal bars for top N functions
(define (render-fuel-bars p n)
  (let* ([hotspots (find-hotspots p n)]
         [max-name-len (apply max (cons 10 (map (lambda (h)
                                                        (string-length (symbol->string (car h))))
                                                hotspots)))]
         [bar-width (- 60 max-name-len 15)])
        (string-append
         "\n  FUEL CONSUMPTION BY FUNCTION\n"
         "  " (make-string 60 #\-) "\n"
         (apply string-append
                (map (lambda (h)
                             (render-bar h max-name-len bar-width))
                     hotspots))
         "\n")))

;;; render-bar : Hotspot × NameWidth × BarWidth → String
(define (render-bar hotspot name-width bar-width)
  (let* ([name (car hotspot)]
         [pct (cadr hotspot)]
         [calls (caddr hotspot)]
         [name-str (symbol->string name)]
         [padded-name (string-append
                       name-str
                       (make-string (- name-width (string-length name-str)) #\space))]
         [filled (round (* bar-width (/ pct 100)))]
         [empty (- bar-width filled)]
         [bar (string-append
               (make-string filled #\#)
               (make-string empty #\-))])
        (format "  ~a |~a| ~a% (~a)\n"
                padded-name bar (round pct) calls)))

;;; ====
;;; Pie Chart (ASCII)
;;; ====

;;; render-pie-chart : Profiler × Nat → String
;;; Render a simple ASCII pie chart legend
(define (render-pie-chart p n)
  (let* ([hotspots (find-hotspots p n)]
         [chars "ABCDEFGHIJ"]
         [legend-lines
          (let loop ([hs hotspots] [i 0] [acc '()])
               (if (or (null? hs) (>= i (string-length chars)))
                   (reverse acc)
                   (let* ([h (car hs)]
                          [ch (string-ref chars i)]
                          [name (car h)]
                          [pct (cadr h)])
                         (loop (cdr hs) (+ i 1)
                               (cons (format "    ~a = ~a (~a%)\n"
                                             ch name (round pct))
                                     acc)))))])
        (string-append
         "\n  FUEL BUDGET DISTRIBUTION\n"
         "  " (make-string 40 #\-) "\n"
         (apply string-append legend-lines)
         "\n")))

;;; ====
;;; Call Tree with Inline Bars
;;; ====

;;; render-tree-with-bars : Profiler → String
;;; Render call tree with inline fuel consumption bars
(define (render-tree-with-bars p)
  (let* ([root (profiler-root p)]
         [total (node-fuel-consumed root)])
        (string-append
         "\n  CALL TREE (with fuel bars)\n"
         "  " (make-string 70 #\=) "\n"
         (tree-lines-with-bars root 0 total)
         "\n")))

;;; tree-lines-with-bars : Node × Depth × TotalFuel → String
(define (tree-lines-with-bars node depth total-fuel)
  (let* ([name (node-name node)]
         [fuel (node-fuel-consumed node)]
         [calls (node-call-count node)]
         [pct (if (zero? total-fuel) 0 (* 100.0 (/ fuel total-fuel)))]
         [bar-width 20]
         [filled (round (* bar-width (/ pct 100)))]
         [bar (string-append (make-string filled #\#)
                             (make-string (- bar-width filled) #\-))]
         [indent (make-string (* depth 2) #\space)]
         [line (format "  ~a~a ~a [~a] ~a% (~a)\n"
                       indent
                       (if (> depth 0) "|- " "")
                       name bar (round pct) calls)]
         [children (reverse (node-children node))]
         [child-lines (map (lambda (c)
                                   (tree-lines-with-bars c (+ depth 1) total-fuel))
                           children)])
        (apply string-append (cons line child-lines))))

;;; ====
;;; Summary Statistics Box
;;; ====

;;; render-summary-box : Profiler → String
(define (render-summary-box p)
  (let* ([stats (profile-stats p)]
         [total (cdr (assq 'total-fuel stats))]
         [used (cdr (assq 'used-fuel stats))]
         [status (profiler-status p)])
        (string-append
         "\n"
         "  +----+\n"
         "  |        PROFILE SUMMARY           |\n"
         "  +----+\n"
         (format "  | Status:      ~a~a|\n"
                 status
                 (make-string (- 19 (string-length (symbol->string status))) #\space))
         (format "  | Fuel Budget: ~a~a|\n"
                 total
                 (make-string (- 19 (string-length (number->string total))) #\space))
         (format "  | Fuel Used:   ~a~a|\n"
                 used
                 (make-string (- 19 (string-length (number->string used))) #\space))
         (format "  | Remaining:   ~a~a|\n"
                 (- total used)
                 (make-string (- 19 (string-length (number->string (- total used)))) #\space))
         "  +----+\n"
         "\n")))

;;; ====
;;; Full Visual Report
;;; ====

;;; render-full-report : Profiler → String
;;; Generate complete visual profile report
(define (render-full-report p)
  (string-append
   (render-summary-box p)
   (render-fuel-bars p 10)
   (render-tree-with-bars p)
   (render-flame-graph p)))

;;; display-profile : Profiler → void
;;; Display full profile visualization
(define (display-profile p)
  (display (render-full-report p)))
