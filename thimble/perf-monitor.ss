;;; shell/perf-monitor.ss — Real-Time Performance Monitoring Dashboard
;;;
;;; Live monitoring of system health: memory, store, sessions, operations.
;;; This is Shell code: impure, tracks state, provides observability.
;;;
;;; Dependencies:
;;;   shell/introspect/timing.ss
;;;   shell/introspect/memory.ss
;;;   shell/store-analyze.ss
;;;   shell/fs.ss
;;;   core/cas.ss
;;;
;;; Operations:
;;;   (start-monitor!) — Start background monitoring
;;;   (stop-monitor!) — Stop monitoring
;;;   (monitor-status) — Display current metrics
;;;   (monitor-history) — Show historical data
;;;   (reset-monitor!) — Reset all metrics
;;;   (monitor-op name thunk) — Track an operation
;;;
;;; Features:
;;;   - Real-time memory tracking
;;;   - Store growth monitoring
;;;   - Operation timing
;;;   - Session metrics
;;;   - Alert thresholds
;;;   - Historical data retention

;;; ============================================================
;;; Configuration
;;; ============================================================

(define *monitor-enabled* #f)
(define *monitor-interval-ms* 5000)  ; Poll every 5 seconds
(define *history-max-samples* 100)   ; Keep last 100 samples

;;; Threshold configuration (for alerts)
(define *memory-threshold-mb* 500)
(define *store-growth-threshold-mb* 100)
(define *slow-op-threshold-ms* 1000)

;;; ============================================================
;;; Monitoring State
;;; ============================================================

;;; Global monitoring state
(define *monitor-state*
  `((enabled . #f)
    (start-time . #f)
    (samples . ())         ; Historical snapshots
    (operations . ())      ; Tracked operations
    (alerts . ())))        ; Active alerts

;;; Sample structure:
;;; (sample timestamp memory-snapshot store-stats session-count)

(define-record-type perf-sample
  (fields
    timestamp-ns
    memory-snapshot
    store-size-bytes
    store-block-count
    session-count
    operation-count))

;;; Operation record:
;;; (operation name start-time duration-ns memory-delta status)

(define-record-type operation-record
  (fields
    name
    timestamp-ns
    duration-ns
    memory-allocated
    status))  ; 'success | 'error

;;; ============================================================
;;; State Management
;;; ============================================================

(define (monitor-enabled?)
  (cdr (assq 'enabled *monitor-state*)))

(define (set-monitor-enabled! enabled?)
  (set! *monitor-state*
    (cons (cons 'enabled enabled?)
          (filter (lambda (p) (not (eq? (car p) 'enabled)))
                  *monitor-state*))))

(define (monitor-start-time)
  (cdr (assq 'start-time *monitor-state*)))

(define (set-monitor-start-time! time)
  (set! *monitor-state*
    (cons (cons 'start-time time)
          (filter (lambda (p) (not (eq? (car p) 'start-time)))
                  *monitor-state*))))

(define (monitor-samples)
  (cdr (assq 'samples *monitor-state*)))

(define (add-monitor-sample! sample)
  (let ([samples (monitor-samples)])
    (set! *monitor-state*
      (cons (cons 'samples
                  (take-last (cons sample samples) *history-max-samples*))
            (filter (lambda (p) (not (eq? (car p) 'samples)))
                    *monitor-state*)))))

(define (monitor-operations)
  (cdr (assq 'operations *monitor-state*)))

(define (add-operation-record! op)
  (let ([ops (monitor-operations)])
    (set! *monitor-state*
      (cons (cons 'operations
                  (take-last (cons op ops) *history-max-samples*))
            (filter (lambda (p) (not (eq? (car p) 'operations)))
                    *monitor-state*)))))

(define (monitor-alerts)
  (cdr (assq 'alerts *monitor-state*)))

(define (add-alert! alert)
  (let ([alerts (monitor-alerts)])
    (set! *monitor-state*
      (cons (cons 'alerts (cons alert alerts))
            (filter (lambda (p) (not (eq? (car p) 'alerts)))
                    *monitor-state*)))))

(define (clear-alerts!)
  (set! *monitor-state*
    (cons (cons 'alerts '())
          (filter (lambda (p) (not (eq? (car p) 'alerts)))
                  *monitor-state*))))

;;; ============================================================
;;; Utilities
;;; ============================================================

(define (take-last lst n)
  "Take last n elements from list"
  (if (<= (length lst) n)
      lst
      (list-tail lst (- (length lst) n))))

(define (current-nanoseconds)
  "Get current time in nanoseconds"
  (let ([t (current-time 'time-monotonic)])
    (+ (* (time-second t) 1000000000)
       (time-nanosecond t))))

(define (format-duration-ns ns)
  "Format nanoseconds as human-readable duration"
  (cond
    [(>= ns 1000000000) (format "~,2fs" (/ ns 1000000000.0))]
    [(>= ns 1000000) (format "~,2fms" (/ ns 1000000.0))]
    [(>= ns 1000) (format "~,2fμs" (/ ns 1000.0))]
    [else (format "~ans" ns)]))

(define (format-bytes bytes)
  "Format bytes as human-readable size"
  (cond
    [(>= (abs bytes) (* 1024 1024 1024))
     (format "~,2fGB" (/ bytes (* 1024.0 1024.0 1024.0)))]
    [(>= (abs bytes) (* 1024 1024))
     (format "~,2fMB" (/ bytes (* 1024.0 1024.0)))]
    [(>= (abs bytes) 1024)
     (format "~,2fKB" (/ bytes 1024.0))]
    [else (format "~aB" bytes)]))

(define (format-timestamp-ns ns)
  "Format timestamp as HH:MM:SS"
  (let* ([seconds (quotient ns 1000000000)]
         [hours (quotient seconds 3600)]
         [minutes (quotient (remainder seconds 3600) 60)]
         [secs (remainder seconds 60)])
    (format "~2,'0d:~2,'0d:~2,'0d" hours minutes secs)))

;;; ============================================================
;;; Sampling
;;; ============================================================

(define (take-sample fs)
  "Capture current performance snapshot"
  (guard (e [else
             ;; If sampling fails, return a minimal sample
             (make-perf-sample
               (current-nanoseconds)
               #f 0 0 0 0)])
    (let* ([mem-snapshot (current-memory-snapshot)]
           [store-size (fs-store-size fs)]
           [block-count (fs-block-count fs)]
           [session-count (get-session-count)])
      (make-perf-sample
        (current-nanoseconds)
        mem-snapshot
        store-size
        block-count
        session-count
        (length (monitor-operations))))))

(define (current-memory-snapshot)
  "Get current memory state - requires introspect/memory.ss"
  (guard (e [else #f])
    (let ([stats (statistics)])
      `((bytes-allocated . ,(bytes-allocated))
        (bytes-in-use . ,(current-memory-bytes))
        (gc-count . ,(sstats-gc-count stats))))))

(define (fs-store-size fs)
  "Get total store size in bytes"
  (guard (e [else 0])
    ;; This would need the fs capability to walk the store
    ;; For now, return 0 as placeholder
    0))

(define (fs-block-count fs)
  "Get total block count in store"
  (guard (e [else 0])
    0))

(define (get-session-count)
  "Get active session count - requires session-manager"
  (guard (e [else 0])
    ;; Call into session-manager if available
    (if (top-level-bound? 'session-count)
        (session-count)
        0)))

;;; ============================================================
;;; Alert System
;;; ============================================================

(define (check-thresholds sample)
  "Check sample against thresholds and generate alerts"
  (let ([alerts '()])
    ;; Check memory threshold
    (when (perf-sample-memory-snapshot sample)
      (let ([mem-mb (/ (cdr (assq 'bytes-in-use
                                  (perf-sample-memory-snapshot sample)))
                       (* 1024 1024))])
        (when (> mem-mb *memory-threshold-mb*)
          (set! alerts (cons
                        `(memory-high
                          ,(format "Memory usage ~,2fMB exceeds threshold ~aMB"
                                   mem-mb *memory-threshold-mb*))
                        alerts)))))

    ;; Return alerts
    alerts))

;;; ============================================================
;;; Operation Tracking
;;; ============================================================

(define (monitor-op name thunk)
  "Execute thunk and track as named operation"
  (let* ([start-time (current-nanoseconds)]
         [mem-before (bytes-allocated)]
         [result #f]
         [status 'success])
    (guard (e [else
               (set! status 'error)
               (raise e)])
      (set! result (thunk)))

    (let* ([end-time (current-nanoseconds)]
           [duration (- end-time start-time)]
           [mem-allocated (- (bytes-allocated) mem-before)]
           [op (make-operation-record name start-time duration mem-allocated status)])

      ;; Record operation
      (add-operation-record! op)

      ;; Check for slow operation alert
      (when (> (/ duration 1000000) *slow-op-threshold-ms*)
        (add-alert! `(slow-operation
                      ,(format "~a took ~a (threshold: ~ams)"
                               name
                               (format-duration-ns duration)
                               *slow-op-threshold-ms*))))

      result)))

;;; ============================================================
;;; Monitoring Control
;;; ============================================================

(define (start-monitor!)
  "Start performance monitoring"
  (when (not (monitor-enabled?))
    (set-monitor-enabled! #t)
    (set-monitor-start-time! (current-nanoseconds))
    (clear-alerts!)
    (display "Performance monitor started.\n")
    (display (format "  Sampling interval: ~ams\n" *monitor-interval-ms*))
    (display (format "  History retention: ~a samples\n" *history-max-samples*))
    (display "\n")
    (display "Use (monitor-status) to view current metrics.\n")))

(define (stop-monitor!)
  "Stop performance monitoring"
  (when (monitor-enabled?)
    (set-monitor-enabled! #f)
    (display "Performance monitor stopped.\n")))

(define (reset-monitor!)
  "Reset all monitoring data"
  (set! *monitor-state*
    `((enabled . ,(monitor-enabled?))
      (start-time . ,(monitor-start-time))
      (samples . ())
      (operations . ())
      (alerts . ())))
  (display "Monitor data reset.\n"))

;;; ============================================================
;;; Status Display
;;; ============================================================

(define (monitor-status)
  "Display current performance metrics"
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║            PERFORMANCE MONITOR - STATUS                     ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n")
  (display "\n")

  ;; Monitor state
  (display (format "Status: ~a\n"
                   (if (monitor-enabled?) "RUNNING" "STOPPED")))

  (when (monitor-start-time)
    (let ([uptime (- (current-nanoseconds) (monitor-start-time))])
      (display (format "Uptime: ~a\n" (format-duration-ns uptime)))))

  (display "\n")

  ;; Current metrics (from latest sample)
  (let ([samples (monitor-samples)])
    (if (null? samples)
        (display "No samples collected yet.\n")
        (let ([latest (car samples)])
          (display "───────────────────────────────────────────────────────────\n")
          (display " CURRENT METRICS\n")
          (display "───────────────────────────────────────────────────────────\n")

          (when (perf-sample-memory-snapshot latest)
            (let ([mem (perf-sample-memory-snapshot latest)])
              (display (format "  Memory Allocated: ~a\n"
                               (format-bytes (cdr (assq 'bytes-allocated mem)))))
              (display (format "  Memory In Use:    ~a\n"
                               (format-bytes (cdr (assq 'bytes-in-use mem)))))
              (display (format "  GC Count:         ~a\n"
                               (cdr (assq 'gc-count mem))))))

          (display (format "  Store Size:       ~a\n"
                           (format-bytes (perf-sample-store-size-bytes latest))))
          (display (format "  Block Count:      ~a\n"
                           (perf-sample-block-count latest)))
          (display (format "  Active Sessions:  ~a\n"
                           (perf-sample-session-count latest)))
          (display (format "  Tracked Ops:      ~a\n"
                           (perf-sample-operation-count latest))))))

  (display "\n")

  ;; Recent operations
  (let ([ops (monitor-operations)])
    (unless (null? ops)
      (display "───────────────────────────────────────────────────────────\n")
      (display " RECENT OPERATIONS (last 5)\n")
      (display "───────────────────────────────────────────────────────────\n")
      (for-each
        (lambda (op)
          (display (format "  ~a ~a ~a ~a\n"
                           (operation-record-name op)
                           (format-duration-ns (operation-record-duration-ns op))
                           (format-bytes (operation-record-memory-allocated op))
                           (if (eq? (operation-record-status op) 'success)
                               "✓"
                               "✗"))))
        (take-last ops 5))
      (display "\n")))

  ;; Active alerts
  (let ([alerts (monitor-alerts)])
    (unless (null? alerts)
      (display "───────────────────────────────────────────────────────────\n")
      (display " ACTIVE ALERTS\n")
      (display "───────────────────────────────────────────────────────────\n")
      (for-each
        (lambda (alert)
          (display (format "  ⚠ ~a: ~a\n" (car alert) (cadr alert))))
        alerts)
      (display "\n"))))

(define (monitor-history)
  "Display historical performance data"
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║            PERFORMANCE MONITOR - HISTORY                    ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n")
  (display "\n")

  (let ([samples (reverse (monitor-samples))])  ; Oldest first
    (if (null? samples)
        (display "No historical data available.\n")
        (begin
          (display (format "Showing ~a samples:\n\n" (length samples)))
          (display "Time       Memory(MB)  Store(MB)  Blocks  Sessions  Ops\n")
          (display "────────────────────────────────────────────────────────\n")
          (for-each
            (lambda (sample)
              (let* ([ts (format-timestamp-ns (perf-sample-timestamp-ns sample))]
                     [mem-mb (if (perf-sample-memory-snapshot sample)
                                 (/ (cdr (assq 'bytes-in-use
                                              (perf-sample-memory-snapshot sample)))
                                    (* 1024.0 1024.0))
                                 0.0)]
                     [store-mb (/ (perf-sample-store-size-bytes sample)
                                  (* 1024.0 1024.0))]
                     [blocks (perf-sample-block-count sample)]
                     [sessions (perf-sample-session-count sample)]
                     [ops (perf-sample-operation-count sample)])
                (display (format "~a  ~6,2f      ~6,2f    ~6a  ~8a  ~3a\n"
                                ts mem-mb store-mb blocks sessions ops))))
            samples)))))

(define (monitor-summary)
  "Display summary statistics"
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║            PERFORMANCE MONITOR - SUMMARY                    ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n")
  (display "\n")

  (let ([ops (monitor-operations)])
    (if (null? ops)
        (display "No operations tracked yet.\n")
        (let* ([total-ops (length ops)]
               [successful (length (filter
                                    (lambda (op)
                                      (eq? (operation-record-status op) 'success))
                                    ops))]
               [failed (- total-ops successful)]
               [durations (map operation-record-duration-ns ops)]
               [total-time (fold-left + 0 durations)]
               [mean-time (/ total-time total-ops)]
               [sorted-times (list-sort < durations)]
               [min-time (car sorted-times)]
               [max-time (list-ref sorted-times (- total-ops 1))])

          (display (format "Total Operations:  ~a\n" total-ops))
          (display (format "Successful:        ~a (~,1f%)\n"
                           successful
                           (* 100.0 (/ successful total-ops))))
          (display (format "Failed:            ~a (~,1f%)\n"
                           failed
                           (* 100.0 (/ failed total-ops))))
          (display "\n")
          (display "Operation Timing:\n")
          (display (format "  Min:  ~a\n" (format-duration-ns min-time)))
          (display (format "  Max:  ~a\n" (format-duration-ns max-time)))
          (display (format "  Mean: ~a\n" (format-duration-ns (inexact->exact (round mean-time)))))
          (display "\n")))))

;;; ============================================================
;;; Quick Helpers
;;; ============================================================

(define (watch-memory)
  "Quick memory snapshot"
  (let ([snap (current-memory-snapshot)])
    (when snap
      (display (format "Memory: ~a allocated, ~a in use, ~a GCs\n"
                       (format-bytes (cdr (assq 'bytes-allocated snap)))
                       (format-bytes (cdr (assq 'bytes-in-use snap)))
                       (cdr (assq 'gc-count snap)))))))

(define (perf-help)
  "Display help for performance monitor"
  (display "
╔══════════════════════════════════════════════════════════════╗
║          PERFORMANCE MONITOR - HELP                          ║
╚══════════════════════════════════════════════════════════════╝

CONTROL:
  (start-monitor!)          Start monitoring
  (stop-monitor!)           Stop monitoring
  (reset-monitor!)          Clear all data

VIEWING:
  (monitor-status)          Show current metrics
  (monitor-history)         Show historical data
  (monitor-summary)         Show summary statistics

TRACKING:
  (monitor-op 'name thunk)  Track an operation
  (watch-memory)            Quick memory check

CONFIGURATION:
  *monitor-interval-ms*        Sampling interval (default: 5000ms)
  *history-max-samples*        Max samples to keep (default: 100)
  *memory-threshold-mb*        Memory alert threshold (default: 500MB)
  *slow-op-threshold-ms*       Slow operation threshold (default: 1000ms)

EXAMPLE:
  (start-monitor!)
  (monitor-op 'my-task (lambda () (do-work)))
  (monitor-status)
  (monitor-summary)
  (stop-monitor!)
"))

;;; ============================================================
;;; Initialization
;;; ============================================================

(display "\n")
(display "Performance Monitor loaded.\n")
(display "Use (perf-help) for command reference.\n")
(display "Use (start-monitor!) to begin monitoring.\n")
(display "\n")
