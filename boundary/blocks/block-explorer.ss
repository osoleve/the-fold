(define-syntax doc
  (syntax-rules ()
    [(_ args ...) (void)]))

;; Search limit (same default as block-navigator.ss)
(define *search-default-limit* 50)

(doc 'module 'block-explorer)
(doc 'description "Interactive Session-Based Block Explorer")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'created-by "builder (sonnet)")
(doc 'note "An interactive block exploration interface that maintains session state, allowing easy navigation through the block store without typing hash prefixes")

(doc 'section 'features)
(doc 'note "Session-based navigation with breadcrumb trail")
(doc 'note "Numbered list navigation (type a number to explore)")
(doc 'note "Quick commands: back, home, search, popular, orphans")
(doc 'note "Maintains current position for easy browsing")
(doc 'note "Integrates with existing block-navigator.ss")

(doc 'section 'session-state)
(doc 'note "Navigation state stored in parameters for session persistence")

(doc current-block-list 'type Parameter)
(doc current-block-list 'description "List of (hash . block) pairs")
(define current-block-list (make-parameter '()))

(doc current-block-hash 'type Parameter)
(doc current-block-hash 'description "Currently viewed block hash")
(define current-block-hash (make-parameter #f))

(doc navigation-history 'type Parameter)
(doc navigation-history 'description "Stack of previous positions")
(define navigation-history (make-parameter '()))

(doc current-mode 'type Parameter)
(doc current-mode 'description "Current mode: home, viewing, search, popular, orphans")
(define current-mode (make-parameter 'home))

(doc 'section 'main-entry)

(define (block-explorer fs)
  (doc 'type (-> FS Void))
  (doc 'description "Start the interactive block explorer, showing home screen")
  (doc 'export #t)
  ;; Reset state
  (current-block-list '())
  (current-block-hash #f)
  (navigation-history '())
  (current-mode 'home)

  (show-home fs))

(define (show-home fs)
  (doc 'type (-> FS Void))
  (doc 'description "Display the home screen with navigation options")
  (current-mode 'home)
  (current-block-hash #f)

  (display "================ INTERACTIVE BLOCK EXPLORER =================\n")
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

(doc 'section 'navigation-commands)

(define (bx-view n)
  (doc 'type (-> Nat Void))
  (doc 'description "View the nth block from the current list")
  (doc 'export #t)
  (let ([blocks (current-block-list)])
       (if (or (null? blocks) (>= n (length blocks)) (< n 0))
           (begin
            (display (format "Invalid selection: ~a\n" n))
            (display (format "Valid range: 0 to ~a\n" (max 0 (- (length blocks) 1)))))
           (let* ([entry (list-ref blocks n)]
                  [hash (car entry)]
                  [blk (cdr entry)])
                 (if (not blk)
                     ;; Block is missing from store
                     (begin
                      (display "===================== MISSING BLOCK ========================\n")
                      (newline)
                      (display (format "Hash: ~a\n\n" (hash->hex hash)))
                      (display "This block is referenced but not present in the store.\n")
                      (display "It may have been deleted or never fully stored.\n\n")
                      (display "Commands:\n")
                      (display "  (bx-back)    - Go back\n")
                      (display "  (bx-home)    - Return to home\n")
                      (newline))
                     ;; Block exists - show details
                     (begin
                      ;; Save current position to history
                      (when (current-block-hash)
                            (navigation-history (cons (current-block-hash) (navigation-history))))

                      ;; Update current position
                      (current-block-hash hash)
                      (current-mode 'viewing)

                      ;; Display the block
                      (show-block-detail (fs) hash blk)))))))

(define (show-block-detail fs hash blk)
  (doc 'type (-> FS Bytevector Block Void))
  (doc 'description "Display a single block with navigation options")
  (display "===================== BLOCK DETAILS =========================\n")
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
       (display "----------------------------------------------------------------\n")
       (display (truncate-string payload-text 400))
       (newline)
       (display "----------------------------------------------------------------\n"))
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

(define (bx-back)
  (doc 'type (-> Void))
  (doc 'description "Go back to the previous block")
  (doc 'export #t)
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

(define (bx-home)
  (doc 'type (-> Void))
  (doc 'description "Return to home screen")
  (doc 'export #t)
  (navigation-history '())
  (show-home (fs)))

(doc 'section 'discovery-commands)

(define (bx-popular)
  (doc 'type (-> Void))
  (doc 'description "Show most popular blocks as numbered list")
  (doc 'export #t)
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

                          (display "================== MOST POPULAR BLOCKS ======================\n")
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

(define (bx-orphans)
  (doc 'type (-> Void))
  (doc 'description "Show orphan blocks as numbered list")
  (doc 'export #t)
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

             (display "===================== ORPHAN BLOCKS =========================\n")
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

(define (bx-search query . args)
  (doc 'type (-> String Void))
  (doc 'description "Search for blocks and display as numbered list. Optional limit arg (default 50, #f for all).")
  (doc 'export #t)
  (current-mode 'search)
  (navigation-history '())
  (current-block-hash #f)

  (let* ([limit (if (null? args) *search-default-limit* (car args))]
         [fs (fs)]
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

        (let* ([all-results (reverse results)]
               [total (length all-results)]
               [display-list (if (and limit (> total limit))
                                 (take limit all-results)
                                 all-results)])

        (display "===================== SEARCH RESULTS ========================\n")
        (newline)
        (display (format "Query: \"~a\"\n" query))
        (if (and limit (> total limit))
            (display (format "Found: ~a blocks (showing first ~a)\n\n" total limit))
            (display (format "Found: ~a blocks\n\n" total)))

        (let loop ([i 0] [result-list display-list])
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

        ;; Store all results for navigation (not just displayed ones)
        (current-block-list all-results)

        (newline)
        (display "Commands:\n")
        (display "  (bx-view N)  - Explore block N\n")
        (display "  (bx-home)    - Return to home\n")
        (newline))))

(doc 'section 'additional-discovery)

(define (bx-recent . args)
  (doc 'type (-> [Nat] Void))
  (doc 'description "Show N most recent blocks (default 10, by hash lexicographic order)")
  (doc 'export #t)
  (let ([n (if (null? args) 10 (car args))])
  (let* ([fs (fs)]
         [all-hashes (fs-all-hashes fs)]
         [sorted (list-sort
                  (lambda (a b)
                          (bytevector<? b a))  ; Reverse sort
                  all-hashes)]
         [recent (take (min n (length sorted)) sorted)])

        (display "==================== RECENT BLOCKS ==========================\n")
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
        (newline))))

(define (bx-by-tag tag . args)
  (doc 'type (-> Symbol [Nat] Void))
  (doc 'description "Show blocks with a specific tag (default limit 50, pass number to change)")
  (doc 'export #t)
  (let* ([limit (if (null? args) *search-default-limit* (car args))]
         [fs (fs)]
         [all-hashes (fs-all-hashes fs)]
         ;; Single pass: fetch once, keep (hash . block) pairs
         [matched-pairs
          (filter-map (lambda (hash)
                        (let ([blk (fs-fetch fs hash)])
                          (and blk (eq? (block-tag blk) tag)
                               (cons hash blk))))
                      all-hashes)]
         [total (length matched-pairs)]
         [display-list (if (> total limit)
                           (take limit matched-pairs)
                           matched-pairs)])

        (display (format "==================== BLOCKS: ~a ====================\n" tag))
        (newline)
        (if (> total limit)
            (display (format "Found ~a blocks with tag '~a (showing first ~a):\n\n" total tag limit))
            (display (format "Found ~a blocks with tag '~a:\n\n" total tag)))

        (let loop ([i 0] [pairs display-list])
             (when (not (null? pairs))
                   (let* ([pair (car pairs)]
                          [hash (car pair)]
                          [blk (cdr pair)])
                         (display (format "  [~a] ~a | ~a bytes\n"
                                          i
                                          (short-hash (hash->hex hash))
                                          (bytevector-length (block-payload blk))))
                         (loop (+ i 1) (cdr pairs)))))

        ;; Store ALL matches for navigation, not just displayed ones
        (current-block-list matched-pairs)

        (newline)
        (when (> total limit)
          (display (format "  ... ~a more blocks. Use (bx-by-tag '~a ~a) to see more.\n\n"
                           (- total limit) tag (* limit 2))))
        (display "Commands:\n")
        (display "  (bx-view N)  - Explore block N (works for all blocks, not just displayed)\n")
        (display "  (bx-home)    - Return to home\n")
        (newline)))

(define (bx-stats)
  (doc 'type (-> Void))
  (doc 'description "Show detailed statistics")
  (doc 'export #t)
  (block-stats (fs)))

(doc 'section 'help)

(define (bx-help)
  (doc 'type (-> Void))
  (doc 'description "Show all commands")
  (doc 'export #t)
  (display "============== BLOCK EXPLORER COMMAND REFERENCE ==============\n")
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

(doc 'section 'helpers)
(doc 'note "Reuse from block-navigator.ss")

(define (short-hash hash-hex)
  (doc 'type (-> String String))
  (doc 'description "Show just the first 8 characters of a hash")
  (substring hash-hex 0 (min 8 (string-length hash-hex))))

(define (truncate-string str max-len)
  (doc 'type (-> String Nat String))
  (doc 'description "Truncate string to max length with ellipsis")
  (if (> (string-length str) max-len)
      (string-append (substring str 0 max-len) "...")
      str))

(define (bn-string-contains-ci? haystack needle)
  (doc 'type (-> String String Boolean))
  (doc 'description "Case-insensitive substring search")
  (let ([hay-lower (string-downcase haystack)]
        [need-lower (string-downcase needle)])
       (bn-string-contains? hay-lower need-lower)))

(define (bn-string-contains? haystack needle)
  (doc 'type (-> String String Boolean))
  (doc 'description "Substring search")
  (let ([need-len (string-length needle)])
       (let loop ([i 0])
            (cond
             [(> (+ i need-len) (string-length haystack)) #f]
             [(string=? (substring haystack i (+ i need-len)) needle) #t]
             [else (loop (+ i 1))]))))

(define (bytevector<? a b)
  (doc 'type (-> Bytevector Bytevector Boolean))
  (doc 'description "Lexicographic comparison of bytevectors")
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
  (doc 'type (-> Nat (List a) (List a)))
  (doc 'description "Take first n elements from list")
  (if (or (= n 0) (null? lst))
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

(doc fs 'type (-> FS))
(doc fs 'description "Get the current filesystem capability")
(doc fs 'note "This function is provided by boundary/repl/repl.ss when loaded in the REPL")
(doc fs 'note "When using this file standalone, you must define (fs) yourself")
(define (fs)
  (mint-fs-capability ".store"))
