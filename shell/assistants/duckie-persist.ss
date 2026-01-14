;;; shell/assistants/duckie-persist.ss — DUCKIE State Persistence to CAS
;;;
;;; Manages DUCKIE's soul in the content-addressed store:
;;;   - Create new DUCKIE and persist initial state
;;;   - Save state changes as new blocks
;;;   - Load DUCKIE from hash
;;;   - Pin active DUCKIE to prevent GC
;;;   - Session management (current active DUCKIE)
;;;
;;; This is Shell code: handles IO and persistence.
;;;
;;; Dependencies:
;;;   - core/cas.ss
;;;   - playpen/duckie.ss

;;; Load dependencies
(source-directories (cons "core" (cons "user" (source-directories))))

(load "core/base/prelude.ss")
(load "core/blocks/block.ss")
(load "core/blocks/cas.ss")
(load "user/duckie.ss")

;;; ====
;;; Session State
;;; ====

;;; Current active DUCKIE (the one we're interacting with)
(define *current-duckie* #f)       ; Duckie struct or #f
(define *current-duckie-hash* #f)  ; Hash of current saved state

;;; Session file location for persistence across daemon restarts
(define *session-file* ".fold-repl/duckie-session.ss")

;;; ====
;;; Persistence Operations
;;; ====

;;; save-duckie! : Duckie → Bytevector
;;; Store DUCKIE to CAS, pin it, return hash.
;;; Creates a new block each time (immutable history).
(define (save-duckie! d)
  (let* ([blk (duckie->block d)]
         [hash (store! blk)])
        (pin! hash)
        ;; Also pin all memory hashes
        (for-each pin! (duckie-memories d))
        hash))

;;; load-duckie : Bytevector → Duckie | #f
;;; Fetch DUCKIE from CAS by hash.
(define (load-duckie hash)
  (let ([blk (fetch hash)])
       (and blk (block->duckie blk))))

;;; duckie-exists? : Bytevector → Boolean
;;; Check if a DUCKIE exists in CAS.
(define (duckie-exists? hash)
  (and (stored? hash)
       (let ([blk (fetch hash)])
            (and blk (eq? (block-tag blk) 'duckie)))))

;;; ====
;;; Session Management
;;; ====

;;; new-duckie! : String → Duckie
;;; Create a new DUCKIE, save to CAS, set as current.
(define (new-duckie! name)
  (let* ([d (make-duckie name)]
         [hash (save-duckie! d)])
        (set! *current-duckie* d)
        (set! *current-duckie-hash* hash)
        (save-session!)
        d))

;;; adopt-duckie! : Bytevector → Duckie | #f
;;; Load an existing DUCKIE from CAS, set as current.
(define (adopt-duckie! hash)
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

;;; ====
;;; State Update with Auto-Save
;;; ====

;;; update-duckie! : (Duckie → Duckie) → Duckie
;;; Apply an update function to current DUCKIE and persist.
;;; Returns the updated DUCKIE.
(define (update-duckie! update-fn)
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

;;; Convenience update functions

;;; pet! : → Duckie
;;; Pet DUCKIE - changes mood, ages, uses a little energy.
(define (pet!)
  (update-duckie!
   (lambda (d)
           (let* ([new-mood (mood-after-interaction (duckie-mood d) 'pet)]
                  [d2 (duckie-set-mood d new-mood)]
                  [d3 (duckie-age-once d2)]
                  [d4 (duckie-drain-energy d3 5)])
                 d4))))

;;; play! : → Duckie
;;; Play with DUCKIE - always results in playful mood.
(define (play!)
  (update-duckie!
   (lambda (d)
           (let* ([d2 (duckie-set-mood d 'playful)]
                  [d3 (duckie-age-once d2)]
                  [d4 (duckie-drain-energy d3 15)])
                 d4))))

;;; rest! : → Duckie
;;; Let DUCKIE rest - restores energy.
(define (rest!)
  (update-duckie!
   (lambda (d)
           (let* ([d2 (duckie-set-mood d 'sleepy)]
                  [d3 (duckie-restore-energy d2 30)])
                 d3))))

;;; feed! : → Duckie
;;; Feed DUCKIE - restores energy, makes happy.
(define (feed!)
  (update-duckie!
   (lambda (d)
           (let* ([d2 (duckie-set-mood d 'happy)]
                  [d3 (duckie-restore-energy d2 50)]
                  [d4 (duckie-age-once d3)])
                 d4))))

;;; idle! : → Duckie
;;; Time passes - mood shifts toward lonely, energy drains.
(define (idle!)
  (update-duckie!
   (lambda (d)
           (let* ([new-mood (mood-after-interaction (duckie-mood d) 'idle)]
                  [d2 (duckie-set-mood d new-mood)]
                  [d3 (duckie-drain-energy d2 2)])
                 d3))))

;;; ====
;;; Memory Operations
;;; ====

;;; remember! : Symbol × Any → Duckie
;;; Add a new memory to DUCKIE.
(define (remember! kind details)
  (update-duckie!
   (lambda (d)
           (let* ([timestamp (current-time)]
                  [mem (make-memory kind timestamp (duckie-mood d) details)]
                  [mem-hash (store! mem)])
                 (pin! mem-hash)
                 (duckie-add-memory d mem-hash)))))

;;; recall : Nat → (List Memory)
;;; Get DUCKIE's last N memories.
(define (recall n)
  (if *current-duckie*
      (let* ([mem-hashes (take n (duckie-memories *current-duckie*))]
             [memories (filter (lambda (x) x) (map fetch mem-hashes))])
            memories)
      '()))

;;; ====
;;; Session Persistence (across daemon restarts)
;;; ====

;;; save-session! : → void
;;; Save current session info to file.
(define (save-session!)
  (when *current-duckie-hash*
        (call-with-output-file *session-file*
                               (lambda (port)
                                       (write `(session (hash . ,(hash->hex *current-duckie-hash*))) port)
                                       (newline port))
                               'replace)))

;;; load-session! : → Duckie | #f
;;; Load session from file, restore DUCKIE.
(define (load-session!)
  (guard (exn [else #f])
         (if (file-exists? *session-file*)
             (call-with-input-file *session-file*
                                   (lambda (port)
                                           (let* ([data (read port)]
                                                  [hash-hex (cdr (assq 'hash (cdr data)))]
                                                  [hash (hex->hash hash-hex)])
                                                 (adopt-duckie! hash))))
             #f)))

;;; ====
;;; Status Display
;;; ====

;;; duckie-status : → void
;;; Display current DUCKIE status.
(define (duckie-status)
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

;;; ====
;;; History Navigation
;;; ====

;;; duckie-history : → (List (cons Bytevector Duckie))
;;; Get list of all saved states for current DUCKIE.
;;; Walks backward through memory refs to find all states.
(define (duckie-history)
  (if (not *current-duckie-hash*)
      '()
      ;; For now, we just have the current state
      ;; Full history tracking would require additional indexing
      (list (cons *current-duckie-hash* *current-duckie*))))

;;; restore-to! : Bytevector → Duckie | #f
;;; Restore DUCKIE to a previous saved state.
(define (restore-to! hash)
  (adopt-duckie! hash))

;;; ====
;;; DUCKIE CAS Statistics
;;; ====

;;; duckie-store-stats : → Alist
;;; Get storage statistics for DUCKIE data.
(define (duckie-store-stats)
  (let ([total (store-count)]
        [gc (gc-stats)])
       `((total-blocks . ,total)
         (pinned . ,(cdr (assq 'pinned gc)))
         (unpinned . ,(cdr (assq 'unpinned gc)))
         (duckie-active . ,(if *current-duckie* 1 0))
         (duckie-memories . ,(if *current-duckie*
                                 (length (duckie-memories *current-duckie*))
                                 0)))))

;;; ====
;;; Help
;;; ====

(define (duckie-help)
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
