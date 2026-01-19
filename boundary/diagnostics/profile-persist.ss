;;; boundary/diagnostics/profile-persist.ss --- Profile Persistence
;;;
;;; Save and load profile data to/from files using S-expression format.
;;; Enables comparison of profiles across sessions and time.
;;;
;;; Usage:
;;;   (profile-save up filename)           ; Save unified profile
;;;   (profile-load filename)              ; Load profile from file
;;;   (profile-save-summary up filename)   ; Save human-readable summary
;;;   (profile-export-csv up filename)     ; Export stats as CSV
;;;
;;; This is Shell code: handles file I/O for profiles.
;;;
;;; Dependencies:
;;;   - boundary/diagnostics/profiler-unified.ss

(load "boundary/diagnostics/profiler-unified.ss")

;;; ====
;;; Save Profile
;;; ====

;;; profile-save : UnifiedProfiler x String -> Boolean
;;; Save a unified profile to a file in S-expression format.
;;; Returns #t on success, #f on failure.
(define (profile-save up filename)
  (guard (ex [else
              (display (format "Error saving profile: ~a\n" ex))
              #f])
         (let ([sexp (unified-profiler->sexp up)])
              (call-with-output-file filename
                                     (lambda (port)
                                             (pretty-print sexp port))
                                     'replace)
              #t)))

;;; profile-save-with-metadata : UnifiedProfiler x String x Alist -> Boolean
;;; Save profile with additional user-provided metadata.
(define (profile-save-with-metadata up filename extra-metadata)
  (guard (ex [else
              (display (format "Error saving profile: ~a\n" ex))
              #f])
         (let* ([sexp (unified-profiler->sexp up)]
                [augmented (augment-metadata sexp extra-metadata)])
               (call-with-output-file filename
                                      (lambda (port)
                                              (pretty-print augmented port))
                                      'replace)
               #t)))

;;; augment-metadata : Sexp x Alist -> Sexp
(define (augment-metadata sexp extra)
  (map (lambda (entry)
               (if (and (pair? entry)
                        (eq? (car entry) 'metadata))
                   `(metadata . ,(append (cdr entry) extra))
                   entry))
       sexp))

;;; ====
;;; Load Profile
;;; ====

;;; profile-load : String -> UnifiedProfileData or #f
;;; Load a profile from file. Returns the raw S-expression data
;;; (not a full UnifiedProfiler, since some state can't be restored).
(define (profile-load filename)
  (guard (ex [else
              (display (format "Error loading profile: ~a\n" ex))
              #f])
         (if (file-exists? filename)
             (call-with-input-file filename read)
             (begin
              (display (format "Profile file not found: ~a\n" filename))
              #f))))

;;; profile-load-stats : String -> Alist or #f
;;; Load profile and extract just the statistics.
(define (profile-load-stats filename)
  (let ([data (profile-load filename)])
       (and data (profile-data-stats data))))

;;; profile-data? : Any -> Boolean
;;; Check if value is loaded profile data
(define (profile-data? x)
  (and (pair? x)
       (eq? (car x) 'unified-profile-data)))

;;; ====
;;; Profile Data Accessors
;;; ====

;;; For loaded profile data (S-expression format)

;;; profile-data-get : ProfileData x Symbol -> Any
(define (profile-data-get data key)
  (let ([entry (assq key (cdr data))])
       (and entry (cdr entry))))

;;; profile-data-version : ProfileData -> Nat
(define (profile-data-version data)
  (profile-data-get data 'version))

;;; profile-data-base : ProfileData -> Alist
(define (profile-data-base data)
  (profile-data-get data 'base-profiler))

;;; profile-data-costs : ProfileData -> Alist
(define (profile-data-costs data)
  (profile-data-get data 'costs))

;;; profile-data-memory : ProfileData -> Alist
(define (profile-data-memory data)
  (profile-data-get data 'memory))

;;; profile-data-call-graph : ProfileData -> Alist
(define (profile-data-call-graph data)
  (profile-data-get data 'call-graph))

;;; profile-data-metadata : ProfileData -> Alist
(define (profile-data-metadata data)
  (profile-data-get data 'metadata))

;;; ====
;;; Extract Statistics from Loaded Data
;;; ====

;;; profile-data-stats : ProfileData -> Alist
;;; Extract comprehensive statistics from loaded profile data.
(define (profile-data-stats data)
  (if (not (profile-data? data))
      #f
      (let* ([base (profile-data-base data)]
             [costs (profile-data-costs data)]
             [memory (profile-data-memory data)]
             [graph (profile-data-call-graph data)]
             [metadata (profile-data-metadata data)]
             
             [fuel-used (node-sexp-total-fuel (cdr (assq 'root base)))]
             [initial-fuel (cdr (assq 'initial-fuel base))]
             [status (cdr (assq 'status base))])
            
            `((fuel . ((used-fuel . ,fuel-used)
                       (total-fuel . ,initial-fuel)
                       (efficiency . ,(if (zero? initial-fuel) 0
                                          (/ fuel-used initial-fuel)))))
              (costs . ,costs)
              (memory . ,memory)
              (call-graph . ,graph)
              (metadata . ,metadata)
              (status . ,status)))))

;;; node-sexp-total-fuel : NodeSexp -> Nat
;;; Calculate total fuel from serialized node
(define (node-sexp-total-fuel node-sexp)
  (if (not (pair? node-sexp))
      0
      (let ([fuel (cdr (assq 'fuel-consumed (cdr node-sexp)))])
           (if fuel fuel 0))))

;;; ====
;;; Save Human-Readable Summary
;;; ====

;;; profile-save-summary : UnifiedProfiler x String -> Boolean
;;; Save a human-readable summary to a text file.
(define (profile-save-summary up filename)
  (guard (ex [else
              (display (format "Error saving summary: ~a\n" ex))
              #f])
         (let ([summary (render-unified-summary up)])
              (call-with-output-file filename
                                     (lambda (port)
                                             (display summary port))
                                     'replace)
              #t)))

;;; profile-save-full-report : UnifiedProfiler x String -> Boolean
;;; Save complete report (summary + all reports) to file.
(define (profile-save-full-report up filename)
  (guard (ex [else
              (display (format "Error saving report: ~a\n" ex))
              #f])
         (let ([report (string-append
                        (render-unified-summary up)
                        (render-memory-report up)
                        (render-cost-report up)
                        (render-call-graph-report up))])
              (call-with-output-file filename
                                     (lambda (port)
                                             (display report port))
                                     'replace)
              #t)))

;;; ====
;;; CSV Export
;;; ====

;;; profile-export-csv : UnifiedProfiler x String -> Boolean
;;; Export profile statistics as CSV for external analysis.
(define (profile-export-csv up filename)
  (guard (ex [else
              (display (format "Error exporting CSV: ~a\n" ex))
              #f])
         (let* ([stats (unified-profile-stats up)]
                [csv-lines (stats->csv stats)])
               (call-with-output-file filename
                                      (lambda (port)
                                              (for-each (lambda (line)
                                                                (display line port)
                                                                (newline port))
                                                        csv-lines))
                                      'replace)
               #t)))

;;; stats->csv : Alist -> List of Strings
;;; Convert statistics to CSV lines
(define (stats->csv stats)
  (let* ([fuel-stats (cdr (assq 'fuel stats))]
         [cost-stats (cdr (assq 'costs stats))]
         [mem-stats (cdr (assq 'memory stats))]
         [graph-stats (cdr (assq 'call-graph stats))]
         
         [header "category,metric,value"]
         
         [fuel-lines
          (list (format "fuel,used,~a" (cdr (assq 'used-fuel fuel-stats)))
                (format "fuel,total,~a" (cdr (assq 'total-fuel fuel-stats)))
                (format "fuel,efficiency,~a" (cdr (assq 'efficiency fuel-stats))))]
         
         [cost-lines
          (let ([breakdown (cdr (assq 'breakdown cost-stats))])
               (map (lambda (entry)
                            (format "cost,~a,~a" (car entry) (cdr entry)))
                    breakdown))]
         
         [mem-lines
          (list (format "memory,total_bytes,~a" (cdr (assq 'total-bytes mem-stats))))]
         
         [graph-lines
          (list (format "graph,nodes,~a" (cdr (assq 'node-count graph-stats)))
                (format "graph,roots,~a" (cdr (assq 'roots graph-stats)))
                (format "graph,leaves,~a" (cdr (assq 'leaves graph-stats)))
                (format "graph,cycles,~a" (cdr (assq 'cycles graph-stats))))])
        
        (cons header
              (append fuel-lines cost-lines mem-lines graph-lines))))

;;; ====
;;; Compare Profiles
;;; ====

;;; profile-compare-loaded : ProfileData x ProfileData -> String
;;; Compare two loaded profiles and return a diff report.
(define (profile-compare-loaded data1 data2)
  (let* ([stats1 (profile-data-stats data1)]
         [stats2 (profile-data-stats data2)]
         
         [fuel1 (cdr (assq 'used-fuel (cdr (assq 'fuel stats1))))]
         [fuel2 (cdr (assq 'used-fuel (cdr (assq 'fuel stats2))))]
         [fuel-diff (- fuel1 fuel2)]
         [fuel-pct (if (zero? fuel1) 0 (* 100.0 (/ fuel-diff fuel1)))]
         
         [mem1 (cdr (assq 'total-bytes (cdr (assq 'memory stats1))))]
         [mem2 (cdr (assq 'total-bytes (cdr (assq 'memory stats2))))]
         [mem-diff (- mem1 mem2)]
         [mem-pct (if (zero? mem1) 0 (* 100.0 (/ mem-diff mem1)))])
        
        (format
         (string-append
          "\n  PROFILE COMPARISON\n"
          "  ====\n\n"
          "  Fuel:\n"
          "    Profile 1: ~a\n"
          "    Profile 2: ~a\n"
          "    Difference: ~a (~a%)\n\n"
          "  Memory:\n"
          "    Profile 1: ~a\n"
          "    Profile 2: ~a\n"
          "    Difference: ~a (~a%)\n")
         fuel1 fuel2 fuel-diff (round fuel-pct)
         (format-bytes mem1) (format-bytes mem2)
         (format-bytes (abs mem-diff)) (round mem-pct))))

;;; profile-load-and-compare : UnifiedProfiler x String -> String
;;; Compare current profile against a saved baseline.
(define (profile-load-and-compare current-up filename)
  (let ([loaded (profile-load filename)])
       (if loaded
           (profile-compare-with-loaded current-up loaded)
           "Could not load baseline profile for comparison.")))

;;; profile-compare-with-loaded : UnifiedProfiler x ProfileData -> String
;;; Compare a live profile with loaded data.
(define (profile-compare-with-loaded up loaded-data)
  (let* ([current-stats (unified-profile-stats up)]
         [loaded-stats (profile-data-stats loaded-data)]
         
         [fuel-current (cdr (assq 'used-fuel (cdr (assq 'fuel current-stats))))]
         [fuel-loaded (cdr (assq 'used-fuel (cdr (assq 'fuel loaded-stats))))]
         [fuel-diff (- fuel-current fuel-loaded)]
         
         [mem-current-data (cdr (assq 'memory current-stats))]
         [mem-loaded-data (cdr (assq 'memory loaded-stats))]
         [mem-current (cdr (assq 'total-bytes mem-current-data))]
         [mem-loaded (cdr (assq 'total-bytes mem-loaded-data))]
         [mem-diff (- mem-current mem-loaded)]
         
         [direction (cond [(> fuel-diff 0) "REGRESSION"]
                          [(< fuel-diff 0) "IMPROVEMENT"]
                          [else "SAME"])])
        
        (format
         (string-append
          "\n  PROFILE vs BASELINE\n"
          "  ====\n\n"
          "  Status: ~a\n\n"
          "  Fuel:\n"
          "    Current:   ~a\n"
          "    Baseline:  ~a\n"
          "    Change:    ~a~a\n\n"
          "  Memory:\n"
          "    Current:   ~a\n"
          "    Baseline:  ~a\n"
          "    Change:    ~a~a\n")
         direction
         fuel-current fuel-loaded
         (if (>= fuel-diff 0) "+" "") fuel-diff
         (format-bytes mem-current) (format-bytes mem-loaded)
         (if (>= mem-diff 0) "+" "") (format-bytes mem-diff))))

;;; ====
;;; File Utilities
;;; ====

;;; profile-file-info : String -> Alist or #f
;;; Get info about a profile file without fully loading it.
(define (profile-file-info filename)
  (let ([data (profile-load filename)])
       (and data
            `((version . ,(profile-data-version data))
              (status . ,(cdr (assq 'status (profile-data-base data))))
              (metadata . ,(profile-data-metadata data))))))

;;; profile-list-saved : String -> List of Strings
;;; List all .profile files in a directory.
(define (profile-list-saved directory)
  (guard (ex [else '()])
         (filter (lambda (f)
                         (let ([len (string-length f)])
                              (and (> len 8)
                                   (string=? (substring f (- len 8) len)
                                             ".profile"))))
                 (directory-list directory))))

;;; ====
;;; Auto-naming
;;; ====

;;; generate-profile-filename : UnifiedProfiler -> String
;;; Generate a timestamped filename for a profile.
(define (generate-profile-filename up)
  (let* ([t (current-time 'time-utc)]
         [d (time-utc->date t)]
         [timestamp (format "~a~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d"
                            (date-year d)
                            (date-month d)
                            (date-day d)
                            (date-hour d)
                            (date-minute d)
                            (date-second d))]
         [status (symbol->string (unified-profiler-status up))])
        (format "profile-~a-~a.profile" timestamp status)))

;;; ====
;;; Module Loading Confirmation
;;; ====

(define *profile-persist-loaded* #t)
