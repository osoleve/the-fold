;;; Load dependencies
(source-directories (cons "core" (cons "user" (source-directories))))

(load "core/base/prelude.ss")
(load "core/blocks/block.ss")
(load "core/blocks/cas.ss")
(load "user/duckie.ss")

(doc 'module 'boundary/assistants/duckie-persist)
(doc 'description "DUCKIE State Persistence to CAS - manages DUCKIE's soul in content-addressed store")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Shell code handling IO and persistence - creates, saves, loads, pins DUCKIE state, manages sessions")

(doc 'section 'session-state)

;;; Current active DUCKIE (the one we're interacting with)
(define *current-duckie* #f)       ; Duckie struct or #f
(define *current-duckie-hash* #f)  ; Hash of current saved state

(define *session-file* ".fold-repl/duckie-session.ss")

(doc 'section 'persistence-operations)

(define (save-duckie! d)
  (doc 'type (-> Duckie Bytevector))
  (doc 'description "Store DUCKIE to CAS, pin it, return hash - creates new block each time for immutable history")
  (let* ([blk (duckie->block d)]
         [hash (store! blk)])
        (pin! hash)
        ;; Also pin all memory hashes
        (for-each pin! (duckie-memories d))
        hash))

(define (load-duckie hash)
  (doc 'type (-> Bytevector (Maybe Duckie)))
  (doc 'description "Fetch DUCKIE from CAS by hash")
  (let ([blk (fetch hash)])
       (and blk (block->duckie blk))))

(define (duckie-exists? hash)
  (doc 'type (-> Bytevector Boolean))
  (doc 'description "Check if a DUCKIE exists in CAS")
  (and (stored? hash)
       (let ([blk (fetch hash)])
            (and blk (eq? (block-tag blk) 'duckie)))))

(doc 'section 'session-management)

(define (new-duckie! name)
  (doc 'type (-> String Duckie))
  (doc 'description "Create a new DUCKIE, save to CAS, set as current")
  (let* ([d (make-duckie name)]
         [hash (save-duckie! d)])
        (set! *current-duckie* d)
        (set! *current-duckie-hash* hash)
        (save-session!)
        d))

(define (adopt-duckie! hash)
  (doc 'type (-> Bytevector (Maybe Duckie)))
  (doc 'description "Load existing DUCKIE from CAS and set as current")
  (let ([d (load-duckie hash)])
       (if d
           (begin
            (set! *current-duckie* d)
            (set! *current-duckie-hash* hash)
            (save-session!)
            d)
           #f)))

;;; current-duckie : → Duckie | #f
;;; Get the currently active DUCKIE.
(define (current-duckie)
  *current-duckie*)

;;; current-duckie-hash : → Bytevector | #f
;;; Get the hash of the current DUCKIE.
(define (current-duckie-hash)
  *current-duckie-hash*)

(doc 'section 'state-update-with-auto-save)

(define (update-duckie! update-fn)
  (doc 'type (-> (-> Duckie Duckie) Duckie))
  (doc 'description "Apply update function to current DUCKIE and persist - returns updated DUCKIE")
  (if *current-duckie*
      (let* ([d (update-fn *current-duckie*)]
             [hash (save-duckie! d)])
            ;; Unpin old state (if different) to allow GC
            (when (and *current-duckie-hash*
                       (not (equal? *current-duckie-hash* hash)))
                  (unpin! *current-duckie-hash*))
            (set! *current-duckie* d)
            (set! *current-duckie-hash* hash)
            d)
      (error "update-duckie!" "No active DUCKIE. Use new-duckie! first.")))

(doc 'section 'convenience-update-functions)

(define (pet!)
  (doc 'type (-> Duckie))
  (doc 'description "Pet DUCKIE - changes mood, ages, uses a little energy")
  (update-duckie!
   (lambda (d)
           (let* ([new-mood (mood-after-interaction (duckie-mood d) 'pet)]
                  [d2 (duckie-set-mood d new-mood)]
                  [d3 (duckie-age-once d2)]
                  [d4 (duckie-drain-energy d3 5)])
                 d4))))

(define (play!)
  (doc 'type (-> Duckie))
  (doc 'description "Play with DUCKIE - always results in playful mood")
  (update-duckie!
   (lambda (d)
           (let* ([d2 (duckie-set-mood d 'playful)]
                  [d3 (duckie-age-once d2)]
                  [d4 (duckie-drain-energy d3 15)])
                 d4))))

(define (rest!)
  (doc 'type (-> Duckie))
  (doc 'description "Let DUCKIE rest - restores energy")
  (update-duckie!
   (lambda (d)
           (let* ([d2 (duckie-set-mood d 'sleepy)]
                  [d3 (duckie-restore-energy d2 30)])
                 d3))))

(define (feed!)
  (doc 'type (-> Duckie))
  (doc 'description "Feed DUCKIE - restores energy, makes happy")
  (update-duckie!
   (lambda (d)
           (let* ([d2 (duckie-set-mood d 'happy)]
                  [d3 (duckie-restore-energy d2 50)]
                  [d4 (duckie-age-once d3)])
                 d4))))

(define (idle!)
  (doc 'type (-> Duckie))
  (doc 'description "Time passes - mood shifts toward lonely, energy drains")
  (update-duckie!
   (lambda (d)
           (let* ([new-mood (mood-after-interaction (duckie-mood d) 'idle)]
                  [d2 (duckie-set-mood d new-mood)]
                  [d3 (duckie-drain-energy d2 2)])
                 d3))))

(doc 'section 'memory-operations)

(define (remember! kind details)
  (doc 'type (-> Symbol Any Duckie))
  (doc 'description "Add a new memory to DUCKIE")
  (update-duckie!
   (lambda (d)
           (let* ([timestamp (current-time)]
                  [mem (make-memory kind timestamp (duckie-mood d) details)]
                  [mem-hash (store! mem)])
                 (pin! mem-hash)
                 (duckie-add-memory d mem-hash)))))

(define (recall n)
  (doc 'type (-> Nat (List Memory)))
  (doc 'description "Get DUCKIE's last N memories")
  (if *current-duckie*
      (let* ([mem-hashes (take n (duckie-memories *current-duckie*))]
             [memories (filter (lambda (x) x) (map fetch mem-hashes))])
            memories)
      '()))

(doc 'section 'session-persistence)

(define (save-session!)
  (doc 'type (-> Void))
  (doc 'description "Save current session info to file for daemon restart persistence")
  (when *current-duckie-hash*
        (call-with-output-file *session-file*
                               (lambda (port)
                                       (write `(session (hash . ,(hash->hex *current-duckie-hash*))) port)
                                       (newline port))
                               'replace)))

(define (load-session!)
  (doc 'type (-> (Maybe Duckie)))
  (doc 'description "Load session from file and restore DUCKIE")
  (guard (exn [else #f])
         (if (file-exists? *session-file*)
             (call-with-input-file *session-file*
                                   (lambda (port)
                                           (let* ([data (read port)]
                                                  [hash-hex (cdr (assq 'hash (cdr data)))]
                                                  [hash (hex->hash hash-hex)])
                                                 (adopt-duckie! hash))))
             #f)))

(doc 'section 'status-display)

(define (duckie-status)
  (doc 'type (-> Void))
  (doc 'description "Display current DUCKIE status")
  (if *current-duckie*
      (let ([d *current-duckie*])
           (display "\n")
           (display "  ╭────────────────────────────────────────────────╮\n")
           (display (format "  │ ~a~a│\n"
                            (duckie-name d)
                            (make-string (max 0 (- 46 (string-length (duckie-name d)))) #\space)))
           (display "  ├────────────────────────────────────────────────┤\n")
           (display (format "  │ Mood:    ~a~a│\n"
                            (duckie-mood d)
                            (make-string (max 0 (- 38 (string-length (symbol->string (duckie-mood d))))) #\space)))
           (display (format "  │ Energy:  ~a/100~a│\n"
                            (duckie-energy d)
                            (make-string (max 0 (- 34 (string-length (number->string (duckie-energy d))))) #\space)))
           (display (format "  │ Age:     ~a interactions~a│\n"
                            (duckie-age d)
                            (make-string (max 0 (- 26 (string-length (number->string (duckie-age d))))) #\space)))
           (display (format "  │ Memories: ~a~a│\n"
                            (length (duckie-memories d))
                            (make-string (max 0 (- 36 (string-length (number->string (length (duckie-memories d)))))) #\space)))
           (when (pair? (duckie-traits d))
                 (display (format "  │ Traits:  ~a~a│\n"
                                  (apply string-append
                                         (map (lambda (t) (string-append (symbol->string t) " "))
                                              (duckie-traits d)))
                                  "")))
           (display "  ╰────────────────────────────────────────────────╯\n")
           (newline))
      (display "\n  No active DUCKIE. Use (new-duckie! \"name\") to create one.\n\n")))

(doc 'section 'history-navigation)

(define (duckie-history)
  (doc 'type (-> (List (Pair Bytevector Duckie))))
  (doc 'description "Get list of all saved states for current DUCKIE")
  (doc 'todo "Full history tracking requires additional indexing")
  (if (not *current-duckie-hash*)
      '()
      ;; For now, we just have the current state
      ;; Full history tracking would require additional indexing
      (list (cons *current-duckie-hash* *current-duckie*))))

(define (restore-to! hash)
  (doc 'type (-> Bytevector (Maybe Duckie)))
  (doc 'description "Restore DUCKIE to a previous saved state")
  (adopt-duckie! hash))

(doc 'section 'duckie-cas-statistics)

(define (duckie-store-stats)
  (doc 'type (-> Alist))
  (doc 'description "Get storage statistics for DUCKIE data")
  (let ([total (store-count)]
        [gc (gc-stats)])
       `((total-blocks . ,total)
         (pinned . ,(cdr (assq 'pinned gc)))
         (unpinned . ,(cdr (assq 'unpinned gc)))
         (duckie-active . ,(if *current-duckie* 1 0))
         (duckie-memories . ,(if *current-duckie*
                                 (length (duckie-memories *current-duckie*))
                                 0)))))

(doc 'section 'help)

(define (duckie-help)
  (doc 'type (-> Void))
  (doc 'description "Display DUCKIE command help")
  (display "\n")
  (display "  ╭────────────────────────────────────────────────────────────╮\n")
  (display "  │                    DUCKIE COMMANDS                         │\n")
  (display "  ├────────────────────────────────────────────────────────────┤\n")
  (display "  │ (new-duckie! name)    Create a new DUCKIE                  │\n")
  (display "  │ (adopt-duckie! hash)  Load existing DUCKIE from CAS        │\n")
  (display "  │ (load-session!)       Restore last session                 │\n")
  (display "  ├────────────────────────────────────────────────────────────┤\n")
  (display "  │ (pet!)                Pet DUCKIE                           │\n")
  (display "  │ (play!)               Play with DUCKIE                     │\n")
  (display "  │ (feed!)               Feed DUCKIE                          │\n")
  (display "  │ (rest!)               Let DUCKIE rest                      │\n")
  (display "  │ (idle!)               Time passes                          │\n")
  (display "  ├────────────────────────────────────────────────────────────┤\n")
  (display "  │ (remember! kind info) Add a memory                         │\n")
  (display "  │ (recall n)            View last n memories                 │\n")
  (display "  ├────────────────────────────────────────────────────────────┤\n")
  (display "  │ (duckie-status)       Show current status                  │\n")
  (display "  │ (current-duckie-hash) Get current save hash                │\n")
  (display "  │ (duckie-store-stats)  Storage statistics                   │\n")
  (display "  ╰────────────────────────────────────────────────────────────╯\n")
  (newline))
