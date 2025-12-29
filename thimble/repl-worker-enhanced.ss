;;; thimble/repl-worker-enhanced.ss — Enhanced REPL Worker with Context-Aware Errors
;;;
;;; Extends the standard worker with The Fold-specific error context
;;; and helpful suggestions for newcomers.

;; Load the original worker first to get all its functionality
(load "thimble/repl-worker.ss")

;; Load our error context system
(load "thimble/error-context-simple.ss")

;;; ============================================================
;;; Enhanced Error Writing
;;; ============================================================

;; Override the error writing function with context-aware version
(set! write-error
      (lambda (path msg)
              (when (file-exists? path)
                    (delete-file path))
              
              ;; Enhance the error message with The Fold context
              (let ([enhanced-msg (enhance-error-message msg)])
                   (call-with-output-file path
                                          (lambda (p)
                                                  (display enhanced-msg p))))))

;;; ============================================================
;;; Error Message Enhancement
;;; ============================================================

(define (enhance-error-message original-error)
  "Enhance error message with The Fold context and suggestions"
  
  ;; Check if this is a The Fold-specific error
  (let ([context (detect-fold-context original-error)]
        [enhanced (format-error-with-fold-context original-error)])
       
       (if (string=? enhanced original-error)
           ;; No specific context found, return original but add general help
           (string-append
            original-error
            "\n\n"
            "💡 Need help? Try:\n"
            "   (help) - Show available functions\n"
            "   (discover) - Find SDKs and tools\n"
            "   (examples 'topic) - See usage examples\n")
           
           ;; We have specific context, use the enhanced message
           enhanced)))

(define (detect-fold-context error-msg)
  "Simple context detection based on error message content"
  (cond
   [(string-contains? error-msg "make-hex-board") 'boardcraft]
   [(string-contains? error-msg "hex-neighbors") 'boardcraft]
   [(string-contains? error-msg "hex-distance") 'boardcraft]
   [(string-contains? error-msg "tilemap") 'loom]
   [(string-contains? error-msg "entity") 'loom]
   [(string-contains? error-msg "help") 'repl-functions]
   [(string-contains? error-msg "axial-coord") 'boardcraft]
   [else 'general]))

;;; ============================================================
;;; Enhanced Request Processing
;;; ============================================================

;; Override the process-request! function to add error context
(set! process-request!
      (lambda (session-id)
              (let ([path (request-path session-id)])
                   (when (file-exists? path)
                         (let* ([content (call-with-input-file path get-string-all)]
                                [request (parse-session-request content)]
                                [expr (if request
                                          (extract-expression request)
                                          content)]
                                [expr-str (if (string? expr)
                                              expr
                                              (format "~s" expr))]
                                [resp-path (response-path session-id)]
                                [err-path (error-path session-id)])
                               (when (file-exists? err-path)
                                     (delete-file err-path))
                               
                               ;; Enhanced error handling with context
                               (guard (e [else
                                          (let ([enhanced-error (enhance-error-message (format-condition e))])
                                               (write-error err-path enhanced-error))])
                                      (let ([result (scheme-eval-and-capture session-id expr-str)])
                                           (write-response resp-path result)))
                               
                               (delete-file path))))))

;;; ============================================================
;;; User-Facing Functions
;;; ============================================================

(define (test-error-context session-id)
  "Test the enhanced error context system"
  (let ([test-file (string-append *requests-dir* "/" session-id "-test.ss")]
        [resp-file (string-append *responses-dir* "/" session-id "-test.txt")]
        [err-file (string-append *responses-dir* "/" session-id "-test.error.txt")])
       
       ;; Write a test expression that will cause an error
       (call-with-output-file test-file
                              (lambda (p)
                                      (display "make-hex-board" p))
                              'replace)
       
       ;; Wait for processing
       (sleep 1)
       
       ;; Check if we got enhanced error
       (if (file-exists? err-file)
           (let ([error-content (call-with-input-file err-file get-string-all)])
                (display "Test error result:\n")
                (display error-content)
                (delete-file test-file)
                (delete-file resp-file)
                (delete-file err-file)
                #t)
           #f)))

(define (get-error-stats session-id)
  "Get statistics about errors in this session"
  (let ([err-path (error-path session-id)])
       (if (file-exists? err-path)
           (let ([content (call-with-input-file err-path get-string-all)])
                (list
                 (cons 'total-errors 1)
                 (cons 'last-error content)
                 (cons 'has-context (string-contains? content "Context:"))))
           '((total-errors . 0)))))

;;; ============================================================
;;; Logging and Analytics
;;; ============================================================

(define *error-log* (make-hash-table))

(define (log-error session-id error-msg context)
  "Log errors for analytics and improvement"
  (let ([timestamp (current-time)]
        [entry (list timestamp error-msg context)])
       (hash-table-update! *error-log* session-id
                           (lambda (errors) (cons entry errors))
                           '())))

(define (get-common-errors)
  "Analyze logged errors to find patterns"
  (let ([all-errors '()])
       (hash-table-for-each
        *error-log*
        (lambda (session-id errors)
                (set! all-errors (append all-errors errors))))
       
       ;; Group by error message and count
       (let ([grouped (group-by (lambda (entry) (list-ref entry 1)) all-errors)])
            (sort (map (lambda (group)
                               (list (car (car group))   ; Error message
                                     (length group)))     ; Count
                       grouped)
                  (lambda (a b) (> (list-ref a 1) (list-ref b 1)))))))

;;; ============================================================
;;; Startup Message
;;; ============================================================

(display "Enhanced REPL worker with context-aware errors loaded!\n")
(display "Errors now provide helpful context and suggestions.\n")
(display "Run (test-error-context 'session-name) to test the system.\n")
