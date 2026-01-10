;;; shell/alloc-tracker.ss --- Allocation Tracking Shim
;;;
;;; Wraps Chez Scheme's statistics for allocation tracking.
;;; Track memory allocations during thunk execution, format sizes,
;;; and accumulate allocation data over time.
;;;
;;; This is Shell code: uses IO, delegates to Chez runtime.
;;;
;;; Usage:
;;;   (get-allocation-stats)              ; Current allocation stats
;;;   (track-allocations thunk)           ; Track allocations during thunk
;;;   (format-bytes n)                    ; Format bytes for display
;;;   (make-alloc-tracker)                ; Create allocation tracker
;;;   (alloc-tracker-record! tracker label bytes)  ; Record an allocation
;;;   (alloc-tracker-summary tracker)     ; Get tracker summary
;;;
;;; Dependencies:
;;;   None (uses Chez built-ins only)

;;; ============================================================
;;; Allocation Statistics
;;; ============================================================

;;; get-allocation-stats : Unit -> Alist
;;; Get current allocation stats from Chez Scheme's statistics.
;;; Returns an alist with relevant memory fields.
(define (get-allocation-stats)
  (let ([s (statistics)])
       `((bytes-allocated . ,(sstats-bytes s))
         (gc-bytes        . ,(sstats-gc-bytes s))
         (gc-count        . ,(sstats-gc-count s)))))

;;; get-bytes-allocated : Unit -> Integer
;;; Get total bytes allocated since program start.
(define (get-bytes-allocated)
  (sstats-bytes (statistics)))

;;; ============================================================
;;; Allocation Tracking
;;; ============================================================

;;; track-allocations : (Unit -> A) -> (values A Integer)
;;; Execute thunk and return both its result and bytes allocated.
;;; Note: Due to GC, this may undercount in long-running operations.
(define (track-allocations thunk)
  (let* ([before (get-bytes-allocated)]
         [result (thunk)]
         [after (get-bytes-allocated)]
         [delta (- after before)])
        (values result (max 0 delta))))  ; Clamp to 0 if GC reduced count

;;; track-allocations/list : (Unit -> A) -> (List A Integer)
;;; Like track-allocations but returns a list instead of values.
;;; Useful when multiple values are inconvenient.
(define (track-allocations/list thunk)
  (call-with-values
   (lambda () (track-allocations thunk))
   list))

;;; with-allocation-report : String x (Unit -> A) -> A
;;; Execute thunk and print allocation report, returning result.
(define (with-allocation-report label thunk)
  (let-values ([(result bytes) (track-allocations thunk)])
              (display (format "~a: ~a allocated\n" label (format-bytes bytes)))
              result))

;;; ============================================================
;;; Byte Formatting
;;; ============================================================

;;; format-bytes : Integer -> String
;;; Format a byte count for human-readable display.
;;; Returns strings like "123 B", "1.5 KB", "2.3 MB", "1.2 GB".
(define (format-bytes n)
  (cond
   [(< n 0) (string-append "-" (format-bytes (- n)))]
   [(< n 1024)
    (format "~a B" n)]
   [(< n (* 1024 1024))
    (format "~a KB" (format-decimal (/ n 1024.0) 1))]
   [(< n (* 1024 1024 1024))
    (format "~a MB" (format-decimal (/ n (* 1024.0 1024.0)) 1))]
   [else
    (format "~a GB" (format-decimal (/ n (* 1024.0 1024.0 1024.0)) 2))]))

;;; format-decimal : Real x Integer -> String
;;; Format a real number with specified decimal places.
;;; Removes trailing zeros for cleaner output.
(define (format-decimal n places)
  (let* ([multiplier (expt 10 places)]
         [rounded (/ (round (* n multiplier)) multiplier)]
         [str (number->string (exact->inexact rounded))])
        ;; Clean up trailing zeros after decimal point
        (clean-decimal-string str)))

;;; clean-decimal-string : String -> String
;;; Remove unnecessary trailing zeros from decimal strings.
;;; "1.50" -> "1.5", "2.00" -> "2", "3.14" -> "3.14"
(define (clean-decimal-string str)
  (let ([has-dot? (let loop ([chars (string->list str)])
                       (cond
                        [(null? chars) #f]
                        [(char=? (car chars) #\.) #t]
                        [else (loop (cdr chars))]))])
       (if (not has-dot?)
           str
           (let loop ([chars (reverse (string->list str))])
                (cond
                 [(null? chars) "0"]
                 [(char=? (car chars) #\0) (loop (cdr chars))]
                 [(char=? (car chars) #\.) (list->string (reverse (cdr chars)))]
                 [else (list->string (reverse chars))])))))

;;; ============================================================
;;; Allocation Tracker Record
;;; ============================================================

;;; Allocation tracker: accumulates labeled allocation records.
;;; Structure: (alloc-tracker entries total-bytes)
;;;   entries: list of (label . bytes) pairs
;;;   total-bytes: running total

;;; make-alloc-tracker : Unit -> AllocTracker
;;; Create a new allocation tracker.
(define (make-alloc-tracker)
  (list 'alloc-tracker '() 0))

;;; alloc-tracker? : Any -> Boolean
;;; Check if value is an allocation tracker.
(define (alloc-tracker? x)
  (and (pair? x)
       (eq? (car x) 'alloc-tracker)
       (= (length x) 3)))

;;; alloc-tracker-entries : AllocTracker -> List
;;; Get the list of (label . bytes) entries.
(define (alloc-tracker-entries tracker)
  (cadr tracker))

;;; alloc-tracker-total : AllocTracker -> Integer
;;; Get total bytes tracked.
(define (alloc-tracker-total tracker)
  (caddr tracker))

;;; alloc-tracker-record! : AllocTracker x String x Integer -> AllocTracker
;;; Record an allocation, returning updated tracker.
;;; Tracker is immutable; returns new tracker.
(define (alloc-tracker-record! tracker label bytes)
  (let ([entries (alloc-tracker-entries tracker)]
        [total (alloc-tracker-total tracker)])
       (list 'alloc-tracker
             (cons (cons label bytes) entries)
             (+ total bytes))))

;;; alloc-tracker-record-thunk! : AllocTracker x String x (Unit -> A) -> (values AllocTracker A)
;;; Execute thunk, record its allocation, return updated tracker and result.
(define (alloc-tracker-record-thunk! tracker label thunk)
  (let-values ([(result bytes) (track-allocations thunk)])
              (values (alloc-tracker-record! tracker label bytes) result)))

;;; ============================================================
;;; Tracker Summaries
;;; ============================================================

;;; alloc-tracker-summary : AllocTracker -> Alist
;;; Get a summary of tracked allocations.
;;; Returns alist with total, count, entries, and formatted total.
(define (alloc-tracker-summary tracker)
  (let ([entries (alloc-tracker-entries tracker)]
        [total (alloc-tracker-total tracker)])
       `((total-bytes     . ,total)
         (total-formatted . ,(format-bytes total))
         (entry-count     . ,(length entries))
         (entries         . ,(reverse entries)))))

;;; alloc-tracker-print-summary : AllocTracker -> Unit
;;; Print a formatted summary of allocations.
(define (alloc-tracker-print-summary tracker)
  (let ([summary (alloc-tracker-summary tracker)])
       (display "\n")
       (display "====== Allocation Summary ======\n")
       (display (format "Total: ~a (~a entries)\n\n"
                        (cdr (assq 'total-formatted summary))
                        (cdr (assq 'entry-count summary))))
       (display "Entries:\n")
       (for-each
        (lambda (entry)
                (display (format "  ~a: ~a\n" (car entry) (format-bytes (cdr entry)))))
        (cdr (assq 'entries summary)))
       (display "================================\n")))

;;; alloc-tracker-top-n : AllocTracker x Integer -> List
;;; Get the top N allocations by size.
(define (alloc-tracker-top-n tracker n)
  (let* ([entries (alloc-tracker-entries tracker)]
         [sorted (list-sort (lambda (a b) (> (cdr a) (cdr b))) entries)])
        (take-at-most sorted n)))

;;; take-at-most : List x Integer -> List
;;; Take at most n elements from a list.
(define (take-at-most lst n)
  (cond
   [(null? lst) '()]
   [(<= n 0) '()]
   [else (cons (car lst) (take-at-most (cdr lst) (- n 1)))]))

;;; ============================================================
;;; Convenience Macros
;;; ============================================================

;;; Syntax for tracking allocations in a block.
;;; (with-tracked-allocations (result bytes) expr ... body ...)
;;; Binds result to the value of expr and bytes to allocations.
(define-syntax with-tracked-allocations
  (syntax-rules ()
                [(_ (result-var bytes-var) expr body ...)
                 (let-values ([(result-var bytes-var) (track-allocations (lambda () expr))])
                             body ...)]))

;;; ============================================================
;;; Module Loading Confirmation
;;; ============================================================

(define *alloc-tracker-loaded* #t)
