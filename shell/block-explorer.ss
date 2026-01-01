;;; thimble/block-explorer.ss — Interactive Session-Based Block Explorer
;;;
;;; Created by builder (sonnet)
;;;
;;; An interactive block exploration interface that maintains session state,
;;; allowing easy navigation through the block store without typing hash prefixes.
;;;
;;; Features:
;;;   - Session-based navigation with breadcrumb trail
;;;   - Numbered list navigation (type a number to explore)
;;;   - Quick commands: back, home, search, popular, orphans
;;;   - Maintains current position for easy browsing
;;;   - Integrates with existing block-navigator.ss
;;;
;;; Usage:
;;;   (block-explorer (fs))       ; Start exploring from home
;;;   (bx-view n)                  ; View block #n from current list
;;;   (bx-back)                    ; Go back to previous block
;;;   (bx-home)                    ; Return to home view
;;;   (bx-search "query")          ; Search and show numbered results
;;;   (bx-popular)                 ; Show popular blocks as numbered list
;;;   (bx-orphans)                 ; Show orphans as numbered list
;;;   (bx-help)                    ; Show command reference

;;; ============================================================
;;; Session State
;;; ============================================================

;;; Navigation state stored in parameters for session persistence
(define current-block-list (make-parameter '()))     ; List of (hash . block) pairs
(define current-block-hash (make-parameter #f))       ; Currently viewed block hash
(define navigation-history (make-parameter '()))      ; Stack of previous positions
(define current-mode (make-parameter 'home))          ; home, viewing, search, popular, orphans

;;; ============================================================
;;; Main Entry Point
;;; ============================================================

;;; block-explorer : FS → void
;;; Start the interactive block explorer, showing home screen.
(define (block-explorer fs)
  ;; Reset state
  (current-block-list '())
  (current-block-hash #f)
  (navigation-history '())
  (current-mode 'home)
  
  (show-home fs))

;;; show-home : FS → void
;;; Display the home screen with navigation options.
(define (show-home fs)
  (current-mode 'home)
  (current-block-hash #f)
  
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║              INTERACTIVE BLOCK EXPLORER                     ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n")
  (newline)
  
  ;; Show quick stats
  (let* ([all-hashes (fs-all-hashes fs)]
         [total (length all-hashes)])
        (display (format "Total blocks in store: ~a\n\n" total))
        
        (display "Navigation Options:\n")
        (display "  (bx-popular)         - View most referenced blocks\n")
        (display "  (bx-orphans)         - View orphan blocks\n")
        (display "  (bx-search \"query\") - Search blocks\n")
        (display "  (bx-recent 10)       - View 10 most recent blocks\n")
        (display "  (bx-by-tag 'tag)     - View blocks by tag\n")
        (display "  (bx-stats)           - Show detailed statistics\n")
        (display "  (bx-help)            - Show all commands\n")
        (newline)))

;;; ============================================================
;;; Navigation Commands
;;; ============================================================

;;; bx-view : Nat → void
;;; View the nth block from the current list.
(define (bx-view n)
  (let ([blocks (current-block-list)])
       (if (or (null? blocks) (>= n (length blocks)) (< n 0))
           (begin
            (display (format "Invalid selection: ~a\n" n))
            (display (format "Valid range: 0 to ~a\n" (max 0 (- (length blocks) 1)))))
           (let* ([entry (list-ref blocks n)]
                  [hash (car entry)]
                  [blk (cdr entry)])
                 ;; Save current position to history
                 (when (current-block-hash)
                       (navigation-history (cons (current-block-hash) (navigation-history))))
                 
                 ;; Update current position
                 (current-block-hash hash)
                 (current-mode 'viewing)
                 
                 ;; Display the block
                 (show-block-detail (fs) hash blk)))))

;;; show-block-detail : FS × Bytevector × Block → void
;;; Display a single block with navigation options.
(define (show-block-detail fs hash blk)
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║                     BLOCK DETAILS                            ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n")
  (newline)
  
  ;; Show breadcrumb
  (let ([depth (length (navigation-history))])
       (when (> depth 0)
             (display (format "📍 Depth: ~a | " (+ depth 1)))
             (display "(bx-back) to return\n\n")))
  
  (display (format "Hash:    ~a\n" (hash->hex hash)))
  (display (format "Tag:     ~a\n" (block-tag blk)))
  (display (format "Payload: ~a bytes\n" (bytevector-length (block-payload blk))))
  (display (format "Refs:    ~a\n" (vector-length (block-refs blk))))
  (newline)
  
  ;; Show payload preview
  (let ([payload-text (guard (e [else "[binary data]"])
                             (utf8->string (block-payload blk)))])
       (display "Payload:\n")
       (display "────────────────────────────────────────────────────────────────\n")
       (display (truncate-string payload-text 400))
       (newline)
       (display "────────────────────────────────────────────────────────────────\n"))
  (newline)
  
  ;; Show refs as numbered list
  (let ([refs (block-refs blk)])
       (when (> (vector-length refs) 0)
             (display "References:\n")
             (let loop ([i 0])
                  (when (< i (vector-length refs))
                        (let* ([ref-hash (vector-ref refs i)]
                               [ref-blk (fs-fetch fs ref-hash)]
                               [tag (if ref-blk (block-tag ref-blk) "[missing]")]
                               [size (if ref-blk (bytevector-length (block-payload ref-blk)) 0)])
                              (display (format "  [~a] ~a | ~a | ~a bytes\n"
                                               i
                                               (short-hash (hash->hex ref-hash))
                                               tag
                                               size)))
                        (loop (+ i 1)))))
       (newline)
       
       ;; Store refs in current-block-list for navigation
       (current-block-list
        (map (lambda (ref-hash)
                     (cons ref-hash (fs-fetch fs ref-hash)))
             (vector->list refs)))
       
       (when (> (vector-length refs) 0)
             (display "Navigation:\n")
             (display "  (bx-view N)  - Explore reference N\n"))
       (display "  (bx-back)    - Go back\n")
       (display "  (bx-home)    - Return to home\n")
       (newline)))

;;; bx-back : () → void
;;; Go back to the previous block.
(define (bx-back)
  (let ([hist (navigation-history)])
       (if (null? hist)
           (begin
            (display "Already at the beginning. Use (bx-home) to return to home.\n")
            (show-home (fs)))
           (let ([prev-hash (car hist)])
                (navigation-history (cdr hist))
                (current-block-hash prev-hash)
                (current-mode 'viewing)
                (let ([blk (fs-fetch (fs) prev-hash)])
                     (if blk
                         (show-block-detail (fs) prev-hash blk)
                         (display "Error: Previous block no longer exists\n")))))))

;;; bx-home : () → void
;;; Return to home screen.
(define (bx-home)
  (navigation-history '())
  (show-home (fs)))

;;; ============================================================
;;; Discovery Commands
;;; ============================================================

;;; bx-popular : () → void
;;; Show most popular blocks as numbered list.
(define (bx-popular)
  (current-mode 'popular)
  (navigation-history '())
  (current-block-hash #f)
  
  (let* ([fs (fs)]
         [all-hashes (fs-all-hashes fs)]
         [ref-counts (make-hashtable equal-hash equal?)])
        
        ;; Count inbound references
        (for-each
         (lambda (hash)
                 (let ([blk (fs-fetch fs hash)])
                      (when blk
                            (let ([refs (block-refs blk)])
                                 (let loop ([i 0])
                                      (when (< i (vector-length refs))
                                            (let* ([ref-hash (vector-ref refs i)]
                                                   [current (hashtable-ref ref-counts ref-hash 0)])
                                                  (hashtable-set! ref-counts ref-hash (+ current 1))
                                                  (loop (+ i 1)))))))))
         all-hashes)
        
        ;; Sort and display
        (let-values ([(hash-vec count-vec) (hashtable-entries ref-counts)])
                    (let* ([hashes (vector->list hash-vec)]
                           [counts (vector->list count-vec)]
                           [pairs (map cons hashes counts)]
                           [sorted (list-sort (lambda (a b) (> (cdr a) (cdr b))) pairs)]
                           [top-20 (take (min 20 (length sorted)) sorted)])
                          
                          (display "╔══════════════════════════════════════════════════════════════╗\n")
                          (display "║                  MOST POPULAR BLOCKS                         ║\n")
                          (display "╚══════════════════════════════════════════════════════════════╝\n")
                          (newline)
                          (display "Blocks ranked by inbound references (top 20):\n\n")
                          
                          (let loop ([i 0] [entries top-20])
                               (when (not (null? entries))
                                     (let* ([pair (car entries)]
                                            [hash (car pair)]
                                            [count (cdr pair)]
                                            [blk (fs-fetch fs hash)])
                                           (display (format "  [~a] ~a refs | ~a | ~a\n"
                                                            i
                                                            count
                                                            (short-hash (hash->hex hash))
                                                            (if blk (block-tag blk) "[missing]")))
                                           (loop (+ i 1) (cdr entries)))))
                          
                          ;; Store in current list
                          (current-block-list
                           (map (lambda (pair)
                                        (let ([hash (car pair)])
                                             (cons hash (fs-fetch fs hash))))
                                top-20))
                          
                          (newline)
                          (display "Commands:\n")
                          (display "  (bx-view N)  - Explore block N\n")
                          (display "  (bx-home)    - Return to home\n")
                          (newline)))))

;;; bx-orphans : () → void
;;; Show orphan blocks as numbered list.
(define (bx-orphans)
  (current-mode 'orphans)
  (navigation-history '())
  (current-block-hash #f)
  
  (let* ([fs (fs)]
         [all-hashes (fs-all-hashes fs)]
         [referenced (make-hashtable equal-hash equal?)])
        
        ;; Mark referenced blocks
        (for-each
         (lambda (hash)
                 (let ([blk (fs-fetch fs hash)])
                      (when blk
                            (let ([refs (block-refs blk)])
                                 (let loop ([i 0])
                                      (when (< i (vector-length refs))
                                            (hashtable-set! referenced (vector-ref refs i) #t)
                                            (loop (+ i 1))))))))
         all-hashes)
        
        ;; Find orphans
        (let ([orphans (filter (lambda (hash)
                                       (not (hashtable-ref referenced hash #f)))
                               all-hashes)])
             
             (display "╔══════════════════════════════════════════════════════════════╗\n")
             (display "║                     ORPHAN BLOCKS                            ║\n")
             (display "╚══════════════════════════════════════════════════════════════╝\n")
             (newline)
             (display (format "Found ~a blocks with no inbound references:\n\n" (length orphans)))
             
             (let loop ([i 0] [orphan-list orphans])
                  (when (not (null? orphan-list))
                        (let* ([hash (car orphan-list)]
                               [blk (fs-fetch fs hash)])
                              (display (format "  [~a] ~a | ~a | ~a bytes\n"
                                               i
                                               (short-hash (hash->hex hash))
                                               (if blk (block-tag blk) "[missing]")
                                               (if blk (bytevector-length (block-payload blk)) 0)))
                              (loop (+ i 1) (cdr orphan-list)))))
             
             ;; Store in current list
             (current-block-list
              (map (lambda (hash)
                           (cons hash (fs-fetch fs hash)))
                   orphans))
             
             (newline)
             (display "Commands:\n")
             (display "  (bx-view N)  - Explore block N\n")
             (display "  (bx-home)    - Return to home\n")
             (newline))))

;;; bx-search : String → void
;;; Search for blocks and display as numbered list.
(define (bx-search query)
  (current-mode 'search)
  (navigation-history '())
  (current-block-hash #f)
  
  (let* ([fs (fs)]
         [all-hashes (fs-all-hashes fs)]
         [results '()])
        
        ;; Search blocks
        (for-each
         (lambda (hash)
                 (let ([blk (fs-fetch fs hash)])
                      (when blk
                            (let* ([tag-str (symbol->string (block-tag blk))]
                                   [payload-str (guard (e [else ""])
                                                       (utf8->string (block-payload blk)))]
                                   [tag-match? (bn-string-contains-ci? tag-str query)]
                                   [payload-match? (bn-string-contains-ci? payload-str query)])
                                  (when (or tag-match? payload-match?)
                                        (set! results (cons (cons hash blk) results)))))))
         all-hashes)
        
        (display "╔══════════════════════════════════════════════════════════════╗\n")
        (display "║                     SEARCH RESULTS                           ║\n")
        (display "╚══════════════════════════════════════════════════════════════╝\n")
        (newline)
        (display (format "Query: \"~a\"\n" query))
        (display (format "Found: ~a blocks\n\n" (length results)))
        
        (let loop ([i 0] [result-list (reverse results)])
             (when (not (null? result-list))
                   (let* ([entry (car result-list)]
                          [hash (car entry)]
                          [blk (cdr entry)]
                          [preview (guard (e [else "[binary]"])
                                          (truncate-string (utf8->string (block-payload blk)) 60))])
                         (display (format "  [~a] ~a | ~a\n"
                                          i
                                          (short-hash (hash->hex hash))
                                          (block-tag blk)))
                         (display (format "       ~a\n" preview))
                         (loop (+ i 1) (cdr result-list)))))
        
        ;; Store in current list
        (current-block-list (reverse results))
        
        (newline)
        (display "Commands:\n")
        (display "  (bx-view N)  - Explore block N\n")
        (display "  (bx-home)    - Return to home\n")
        (newline)))

;;; ============================================================
;;; Additional Discovery Commands
;;; ============================================================

;;; bx-recent : Nat → void
;;; Show N most recent blocks (by hash lexicographic order - approximation).
(define (bx-recent n)
  (let* ([fs (fs)]
         [all-hashes (fs-all-hashes fs)]
         [sorted (list-sort
                  (lambda (a b)
                          (bytevector<? b a))  ; Reverse sort
                  all-hashes)]
         [recent (take (min n (length sorted)) sorted)])
        
        (display "╔══════════════════════════════════════════════════════════════╗\n")
        (display "║                    RECENT BLOCKS                             ║\n")
        (display "╚══════════════════════════════════════════════════════════════╝\n")
        (newline)
        (display (format "Showing ~a most recent blocks:\n\n" (length recent)))
        
        (let loop ([i 0] [hash-list recent])
             (when (not (null? hash-list))
                   (let* ([hash (car hash-list)]
                          [blk (fs-fetch fs hash)])
                         (display (format "  [~a] ~a | ~a | ~a bytes\n"
                                          i
                                          (short-hash (hash->hex hash))
                                          (if blk (block-tag blk) "[missing]")
                                          (if blk (bytevector-length (block-payload blk)) 0)))
                         (loop (+ i 1) (cdr hash-list)))))
        
        (current-block-list
         (map (lambda (hash)
                      (cons hash (fs-fetch fs hash)))
              recent))
        
        (newline)
        (display "Commands:\n")
        (display "  (bx-view N)  - Explore block N\n")
        (display "  (bx-home)    - Return to home\n")
        (newline)))

;;; bx-by-tag : Symbol → void
;;; Show all blocks with a specific tag.
(define (bx-by-tag tag)
  (let* ([fs (fs)]
         [all-hashes (fs-all-hashes fs)]
         [matches (filter
                   (lambda (hash)
                           (let ([blk (fs-fetch fs hash)])
                                (and blk (eq? (block-tag blk) tag))))
                   all-hashes)])
        
        (display "╔══════════════════════════════════════════════════════════════╗\n")
        (display (format "║                    BLOCKS: ~a~a║\n"
                         tag
                         (make-string (max 0 (- 42 (string-length (symbol->string tag)))) #\space)))
        (display "╚══════════════════════════════════════════════════════════════╝\n")
        (newline)
        (display (format "Found ~a blocks with tag '~a:\n\n" (length matches) tag))
        
        (let loop ([i 0] [hash-list matches])
             (when (not (null? hash-list))
                   (let* ([hash (car hash-list)]
                          [blk (fs-fetch fs hash)])
                         (display (format "  [~a] ~a | ~a bytes\n"
                                          i
                                          (short-hash (hash->hex hash))
                                          (if blk (bytevector-length (block-payload blk)) 0)))
                         (loop (+ i 1) (cdr hash-list)))))
        
        (current-block-list
         (map (lambda (hash)
                      (cons hash (fs-fetch fs hash)))
              matches))
        
        (newline)
        (display "Commands:\n")
        (display "  (bx-view N)  - Explore block N\n")
        (display "  (bx-home)    - Return to home\n")
        (newline)))

;;; bx-stats : () → void
;;; Show detailed statistics.
(define (bx-stats)
  (block-stats (fs)))

;;; ============================================================
;;; Help
;;; ============================================================

;;; bx-help : () → void
;;; Show all commands.
(define (bx-help)
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║            BLOCK EXPLORER COMMAND REFERENCE                  ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n")
  (newline)
  (display "Starting:\n")
  (display "  (block-explorer (fs))  - Start the explorer\n")
  (newline)
  (display "Discovery:\n")
  (display "  (bx-popular)           - View most referenced blocks\n")
  (display "  (bx-orphans)           - View orphan blocks\n")
  (display "  (bx-search \"query\")   - Search blocks by content\n")
  (display "  (bx-recent N)          - View N recent blocks\n")
  (display "  (bx-by-tag 'tag)       - View blocks by tag\n")
  (display "  (bx-stats)             - Show detailed statistics\n")
  (newline)
  (display "Navigation:\n")
  (display "  (bx-view N)            - View/explore block number N\n")
  (display "  (bx-back)              - Go back to previous block\n")
  (display "  (bx-home)              - Return to home screen\n")
  (newline)
  (display "Tips:\n")
  (display "  - Use discovery commands to find interesting blocks\n")
  (display "  - Use (bx-view N) to drill down into references\n")
  (display "  - Use (bx-back) to navigate back through your trail\n")
  (display "  - Each list shows numbered blocks you can explore\n")
  (newline))

;;; ============================================================
;;; Helper Functions
;;; ============================================================

;;; Reuse from block-navigator.ss
(define (short-hash hash-hex)
  (substring hash-hex 0 (min 8 (string-length hash-hex))))

(define (truncate-string str max-len)
  (if (> (string-length str) max-len)
      (string-append (substring str 0 max-len) "...")
      str))

(define (bn-string-contains-ci? haystack needle)
  (let ([hay-lower (string-downcase haystack)]
        [need-lower (string-downcase needle)])
       (bn-string-contains? hay-lower need-lower)))

(define (bn-string-contains? haystack needle)
  (let ([need-len (string-length needle)])
       (let loop ([i 0])
            (cond
             [(> (+ i need-len) (string-length haystack)) #f]
             [(string=? (substring haystack i (+ i need-len)) needle) #t]
             [else (loop (+ i 1))]))))

(define (bytevector<? a b)
  (let ([len-a (bytevector-length a)]
        [len-b (bytevector-length b)])
       (let loop ([i 0])
            (cond
             [(= i len-a) (< len-a len-b)]
             [(= i len-b) #f]
             [(< (bytevector-u8-ref a i) (bytevector-u8-ref b i)) #t]
             [(> (bytevector-u8-ref a i) (bytevector-u8-ref b i)) #f]
             [else (loop (+ i 1))]))))

(define (take n lst)
  (if (or (= n 0) (null? lst))
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

;;; fs : () → FS
;;; Get the current filesystem capability.
;;; This function is provided by shell/repl.ss when loaded in the REPL.
;;; When using this file standalone, you must define (fs) yourself.
;;; Default implementation for REPL:
(define (fs)
  (mint-fs-capability ".store"))
