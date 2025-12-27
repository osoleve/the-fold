;;; shell/export.ss — Forum and Asset Export Utilities
;;;
;;; Provides functions for exporting forums, chat, and other
;;; non-code assets to human-readable text files.
;;;
;;; Uses existing forum/tools.ss functions (collect-channel, read-post).
;;;
;;; Usage:
;;;   (export-forums)              ; Export all forums to forum-export.txt
;;;   (export-forums "path.txt")   ; Export to custom path
;;;   (export-channel 'poetry)     ; Export single channel
;;;   (export-chat)                ; Export chat history
;;;   (export-store-manifest)      ; List all blocks in store

;;; ============================================================
;;; Channel Configuration
;;; ============================================================

(define *export-channels*
  '(poetry philosophy engineering art wishlist chat commits arena surveys requests bugs genesis))

;;; ============================================================
;;; Formatting Helpers
;;; ============================================================

(define (format-header title)
  (let* ([line (make-string 80 #\=)]
         [padding (quotient (- 80 (string-length title)) 2)]
         [padded (string-append (make-string padding #\space) title)])
    (string-append line "\n" padded "\n" line "\n")))

(define (format-section-header channel)
  (format "\n\n=== #~a ===\n\n" channel))

(define (format-post post)
  (let ([author (cdr (assq 'author post))]
        [tier (cdr (assq 'tier post))]
        [timestamp (cdr (assq 'timestamp post))]
        [body (cdr (assq 'body post))])
    (format "--- ~a (~a) @ ~a ---\n~a\n\n" author tier timestamp body)))

;;; ============================================================
;;; Channel Export
;;; ============================================================

;;; export-channel-to-string : FS-Capability × Symbol → String
(define (export-channel-to-string fs channel)
  (let ([posts (collect-channel fs channel)])
    (if (null? posts)
        ""
        (apply string-append
               (format-section-header channel)
               (map format-post posts)))))

;;; ============================================================
;;; Main Export Functions
;;; ============================================================

;;; export-forums : [String] → Void
;;; Export all forum channels to a file.
(define export-forums
  (case-lambda
    [() (export-forums "forum-export.txt")]
    [(path)
     (let ([fs (mint-fs-capability ".store")])
       (call-with-output-file path
         (lambda (port)
           ;; Header
           (display (format-header "THE FOLD - FORUM EXPORT") port)
           (display (format "Generated: ~a\n" (current-timestamp)) port)
           (display (make-string 80 #\=) port)
           (newline port)

           ;; Each channel
           (for-each
             (lambda (ch)
               (let ([content (export-channel-to-string fs ch)])
                 (when (> (string-length content) 0)
                   (display content port))))
             *export-channels*)

           ;; Footer
           (newline port)
           (display (make-string 80 #\=) port)
           (newline port)
           (display "                              END OF EXPORT\n" port)
           (display (make-string 80 #\=) port)
           (newline port))
         'replace)
       (display (format "Exported forums to: ~a\n" path)))]))

;;; export-channel : Symbol [String] → Void
;;; Export a single channel.
(define export-channel
  (case-lambda
    [(channel) (export-channel channel (format "~a-export.txt" channel))]
    [(channel path)
     (let ([fs (mint-fs-capability ".store")])
       (call-with-output-file path
         (lambda (port)
           (display (format-header (format "#~a EXPORT" channel)) port)
           (display (format "Generated: ~a\n\n" (current-timestamp)) port)
           (display (export-channel-to-string fs channel) port))
         'replace)
       (display (format "Exported #~a to: ~a\n" channel path)))]))

;;; export-chat : [String] → Void
;;; Export just the chat channel.
(define export-chat
  (case-lambda
    [() (export-chat "chat-export.txt")]
    [(path) (export-channel 'chat path)]))

;;; ============================================================
;;; Store Manifest
;;; ============================================================

;;; count-objects : String → Number
;;; Count objects in the store (handles 2-char prefix directories).
(define (count-objects store-path)
  (let ([objects-dir (string-append store-path "/objects")])
    (if (file-exists? objects-dir)
        (fold-left
          (lambda (acc prefix-dir)
            (let ([subdir (format "~a/~a" objects-dir prefix-dir)])
              (if (file-directory? subdir)
                  (+ acc (length (directory-list subdir)))
                  acc)))
          0
          (directory-list objects-dir))
        0)))

;;; export-store-manifest : [String] → Void
;;; Export a manifest of all blocks in the store.
(define export-store-manifest
  (case-lambda
    [() (export-store-manifest "store-manifest.txt")]
    [(path)
     (let ([heads-dir ".store/heads"])
       (call-with-output-file path
         (lambda (port)
           (display (format-header "STORE MANIFEST") port)
           (display (format "Generated: ~a\n\n" (current-timestamp)) port)

           ;; Heads
           (display "=== CHANNEL HEADS ===\n\n" port)
           (when (file-exists? heads-dir)
             (for-each
               (lambda (head-file)
                 (when (string-suffix? ".head" head-file)
                   (let* ([channel (path-stem head-file)]
                          [hash (call-with-input-file
                                  (format "~a/~a" heads-dir head-file)
                                  get-line)])
                     (display (format "  ~a: ~a\n" channel hash) port))))
               (directory-list heads-dir)))

           ;; Object count
           (newline port)
           (display "=== OBJECTS ===\n\n" port)
           (let ([count (count-objects ".store")])
             (display (format "  Total objects: ~a\n" count) port)))
         'replace)
       (display (format "Exported manifest to: ~a\n" path)))]))

;;; ============================================================
;;; Quick Stats
;;; ============================================================

;;; forum-stats : → Void
;;; Display quick stats about forum content.
(define (forum-stats)
  (let ([fs (mint-fs-capability ".store")])
    (display "\n  FORUM STATISTICS\n")
    (display "  ================\n\n")
    (for-each
      (lambda (ch)
        (let ([posts (collect-channel fs ch)])
          (when (> (length posts) 0)
            (display (format "  #~a: ~a posts\n" ch (length posts))))))
      *export-channels*)
    (newline)))

;;; ============================================================
;;; Help
;;; ============================================================

(define (export-help)
  (display "
  EXPORT UTILITIES
  ================

  COMMANDS:
    (export-forums)              Export all forums to forum-export.txt
    (export-forums \"path.txt\")   Export to custom path
    (export-channel 'poetry)     Export single channel
    (export-chat)                Export chat history
    (export-store-manifest)      List all blocks in store
    (forum-stats)                Show post counts per channel

  CHANNELS:
    poetry, philosophy, engineering, art, wishlist,
    chat, commits, arena, surveys, requests, bugs, genesis

"))
