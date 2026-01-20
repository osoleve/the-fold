(doc 'module 'alloc-tracker)
(doc 'description "Allocation Tracking Shim - wraps Chez Scheme's statistics for allocation tracking")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'note "Track memory allocations during thunk execution, format sizes, and accumulate allocation data over time")

(doc 'section 'allocation-statistics)

(define (get-allocation-stats)
  (doc 'description "Get current allocation stats from Chez Scheme's statistics")
  (doc 'returns "Alist with relevant memory fields")

  (let ([s (statistics)])
       `((bytes-allocated . ,(sstats-bytes s))
         (gc-bytes        . ,(sstats-gc-bytes s))
         (gc-count        . ,(sstats-gc-count s)))))

(define (get-bytes-allocated)
  (doc 'description "Get total bytes allocated since program start")
  (doc 'returns "Integer - bytes allocated")

  (sstats-bytes (statistics)))

(doc 'section 'allocation-tracking)

(define (track-allocations thunk)
  (doc 'description "Execute thunk and return both its result and bytes allocated")
  (doc 'param '(thunk "Function to execute"))
  (doc 'returns "values - result and bytes allocated")
  (doc 'note "Due to GC, this may undercount in long-running operations")

  (let* ([before (get-bytes-allocated)]
         [result (thunk)]
         [after (get-bytes-allocated)]
         [delta (- after before)])
        (values result (max 0 delta))))

(define (track-allocations/list thunk)
  (doc 'description "Like track-allocations but returns a list instead of values")
  (doc 'param '(thunk "Function to execute"))
  (doc 'returns "List of result and bytes allocated")

  (call-with-values
   (lambda () (track-allocations thunk))
   list))

(define (with-allocation-report label thunk)
  (doc 'description "Execute thunk and print allocation report, returning result")
  (doc 'param '(label "Label for report"))
  (doc 'param '(thunk "Function to execute"))
  (doc 'returns "Result of thunk")

  (let-values ([(result bytes) (track-allocations thunk)])
              (display (format "~a: ~a allocated\n" label (format-bytes bytes)))
              result))

(doc 'section 'byte-formatting)

(define (format-bytes n)
  (doc 'description "Format a byte count for human-readable display")
  (doc 'param '(n "Number of bytes"))
  (doc 'returns "String like \"123 B\", \"1.5 KB\", \"2.3 MB\", \"1.2 GB\"")

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

(define (format-decimal n places)
  (doc 'description "Format a real number with specified decimal places")
  (doc 'param '(n "Number to format"))
  (doc 'param '(places "Number of decimal places"))
  (doc 'returns "String - formatted number with trailing zeros removed")

  (let* ([multiplier (expt 10 places)]
         [rounded (/ (round (* n multiplier)) multiplier)]
         [str (number->string (exact->inexact rounded))])
        (clean-decimal-string str)))

(define (clean-decimal-string str)
  (doc 'description "Remove unnecessary trailing zeros from decimal strings")
  (doc 'param '(str "String to clean"))
  (doc 'returns "String - \"1.50\" -> \"1.5\", \"2.00\" -> \"2\", \"3.14\" -> \"3.14\"")

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

(doc 'section 'allocation-tracker-record)

(define (make-alloc-tracker)
  (doc 'description "Create a new allocation tracker")
  (doc 'returns "AllocTracker - structure: (alloc-tracker entries total-bytes)")

  (list 'alloc-tracker '() 0))

(define (alloc-tracker? x)
  (doc 'description "Check if value is an allocation tracker")
  (doc 'param '(x "Value to check"))
  (doc 'returns "Boolean")

  (and (pair? x)
       (eq? (car x) 'alloc-tracker)
       (= (length x) 3)))

(define (alloc-tracker-entries tracker)
  (doc 'description "Get the list of (label . bytes) entries")
  (doc 'param '(tracker "Allocation tracker"))
  (doc 'returns "List of (label . bytes) pairs")

  (cadr tracker))

(define (alloc-tracker-total tracker)
  (doc 'description "Get total bytes tracked")
  (doc 'param '(tracker "Allocation tracker"))
  (doc 'returns "Integer - total bytes")

  (caddr tracker))

(define (alloc-tracker-record! tracker label bytes)
  (doc 'description "Record an allocation, returning updated tracker")
  (doc 'param '(tracker "Allocation tracker"))
  (doc 'param '(label "Label for allocation"))
  (doc 'param '(bytes "Number of bytes allocated"))
  (doc 'returns "AllocTracker - new tracker (immutable)")

  (let ([entries (alloc-tracker-entries tracker)]
        [total (alloc-tracker-total tracker)])
       (list 'alloc-tracker
             (cons (cons label bytes) entries)
             (+ total bytes))))

(define (alloc-tracker-record-thunk! tracker label thunk)
  (doc 'description "Execute thunk, record its allocation, return updated tracker and result")
  (doc 'param '(tracker "Allocation tracker"))
  (doc 'param '(label "Label for allocation"))
  (doc 'param '(thunk "Function to execute"))
  (doc 'returns "values - updated tracker and result")

  (let-values ([(result bytes) (track-allocations thunk)])
              (values (alloc-tracker-record! tracker label bytes) result)))

(doc 'section 'tracker-summaries)

(define (alloc-tracker-summary tracker)
  (doc 'description "Get a summary of tracked allocations")
  (doc 'param '(tracker "Allocation tracker"))
  (doc 'returns "Alist with total, count, entries, and formatted total")

  (let ([entries (alloc-tracker-entries tracker)]
        [total (alloc-tracker-total tracker)])
       `((total-bytes     . ,total)
         (total-formatted . ,(format-bytes total))
         (entry-count     . ,(length entries))
         (entries         . ,(reverse entries)))))

(define (alloc-tracker-print-summary tracker)
  (doc 'description "Print a formatted summary of allocations")
  (doc 'param '(tracker "Allocation tracker"))
  (doc 'returns "void")

  (let ([summary (alloc-tracker-summary tracker)])
       (display "\n")
       (display "==== Allocation Summary ====\n")
       (display (format "Total: ~a (~a entries)\n\n"
                        (cdr (assq 'total-formatted summary))
                        (cdr (assq 'entry-count summary))))
       (display "Entries:\n")
       (for-each
        (lambda (entry)
                (display (format "  ~a: ~a\n" (car entry) (format-bytes (cdr entry)))))
        (cdr (assq 'entries summary)))
       (display "====\n")))

(define (alloc-tracker-top-n tracker n)
  (doc 'description "Get the top N allocations by size")
  (doc 'param '(tracker "Allocation tracker"))
  (doc 'param '(n "Number of top entries to return"))
  (doc 'returns "List of (label . bytes) pairs")

  (let* ([entries (alloc-tracker-entries tracker)]
         [sorted (list-sort (lambda (a b) (> (cdr a) (cdr b))) entries)])
        (take-at-most sorted n)))

(define (take-at-most lst n)
  (doc 'description "Take at most n elements from a list")
  (cond
   [(null? lst) '()]
   [(<= n 0) '()]
   [else (cons (car lst) (take-at-most (cdr lst) (- n 1)))]))

(doc 'section 'convenience-macros)

(define-syntax with-tracked-allocations
  (doc 'description "Syntax for tracking allocations in a block")
  (doc 'note "Usage: (with-tracked-allocations (result bytes) expr body ...)")

  (syntax-rules ()
                [(_ (result-var bytes-var) expr body ...)
                 (let-values ([(result-var bytes-var) (track-allocations (lambda () expr))])
                             body ...)]))

(define *alloc-tracker-loaded* #t)
