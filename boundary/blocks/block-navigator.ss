(define-syntax doc
  (syntax-rules ()
    [(_ args ...) (void)]))

(doc 'module 'block-navigator)
(doc 'description "Interactive Block Store Navigator & Analytics")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'created-by "Secret-Builder (sonnet)")
(doc 'note "A comprehensive tool for exploring and analyzing the content-addressed block store")
(doc 'note "Provides navigation, visualization, analytics, and search capabilities")
(doc 'note "This is Shell code: uses filesystem capabilities, pretty-printing, stats")

(doc 'section 'dependencies)
(doc 'note "core/block.ss, core/sha256.ss, boundary/io/fs.ss must be loaded first")

(doc 'section 'display)

(define (describe-block fs hash)
  (doc 'type (-> FS Bytevector Void))
  (doc 'description "Display detailed information about a single block")
  (doc 'export #t)
  (let ([blk (fs-fetch fs hash)])
       (if (not blk)
           (display (format "Block not found: ~a\n" (hash->hex hash)))
           (begin
            (display "╔══════════════════════════════════════════════════════════════╗\n")
            (display "║                     BLOCK INFORMATION                        ║\n")
            (display "╚══════════════════════════════════════════════════════════════╝\n")
            (display (format "Hash:    ~a\n" (hash->hex hash)))
            (display (format "Tag:     ~a\n" (block-tag blk)))
            (display (format "Payload: ~a bytes\n" (bytevector-length (block-payload blk))))
            (display (format "Refs:    ~a\n" (vector-length (block-refs blk))))
            (newline)

            ;; Show payload preview (first 200 chars)
            (let ([payload-text (guard (e [else "[binary data]"])
                                       (utf8->string (block-payload blk)))])
                 (display "Payload preview:\n")
                 (display (truncate-string payload-text 200))
                 (newline))
            (newline)

            ;; Show refs
            (let ([refs (block-refs blk)])
                 (when (> (vector-length refs) 0)
                       (display "References:\n")
                       (let loop ([i 0])
                            (when (< i (vector-length refs))
                                  (display (format "  [~a] ~a\n" i (hash->hex (vector-ref refs i))))
                                  (loop (+ i 1))))))
            (newline)))))

(define (truncate-string str max-len)
  (doc 'type (-> String Nat String))
  (doc 'description "Truncate string to max length with ellipsis")
  (if (> (string-length str) max-len)
      (string-append (substring str 0 max-len) "...")
      str))

(doc 'section 'exploration)

(define (explore fs hash-prefix)
  (doc 'type (-> FS String Void))
  (doc 'description "Explore a block by hash prefix, showing it and its immediate refs")
  (doc 'export #t)
  (let ([hash (find-block-by-prefix fs hash-prefix)])
       (if (not hash)
           (display (format "No block found with prefix: ~a\n" hash-prefix))
           (begin
            (describe-block fs hash)

            ;; Offer to explore refs
            (let ([blk (fs-fetch fs hash)])
                 (let ([refs (block-refs blk)])
                      (when (> (vector-length refs) 0)
                            (display "Use (explore-ref fs \"hash-prefix\" n) to explore reference n\n"))))))))

(define (explore-ref fs hash-prefix n)
  (doc 'type (-> FS String Nat Void))
  (doc 'description "Explore the nth reference of a block")
  (doc 'export #t)
  (let ([hash (find-block-by-prefix fs hash-prefix)])
       (if (not hash)
           (display (format "No block found with prefix: ~a\n" hash-prefix))
           (let ([blk (fs-fetch fs hash)])
                (let ([refs (block-refs blk)])
                     (if (>= n (vector-length refs))
                         (display (format "Reference index ~a out of bounds (max: ~a)\n"
                                          n (- (vector-length refs) 1)))
                         (explore fs (hash->hex (vector-ref refs n)))))))))

(define (find-block-by-prefix fs prefix)
  (doc 'type (-> FS String (Maybe Bytevector)))
  (doc 'description "Find a block hash that starts with the given prefix")
  (let ([all-hashes (fs-all-hashes fs)])
       (let loop ([hashes all-hashes])
            (if (null? hashes)
                #f
                (let ([hash (car hashes)])
                     (if (string-prefix? prefix (hash->hex hash))
                         hash
                         (loop (cdr hashes))))))))

(doc 'note "string-prefix? is now provided by boundary/io/fs.ss")

(doc 'section 'visualization)

(define (visualize-tree fs hash-prefix max-depth)
  (doc 'type (-> FS String Nat Void))
  (doc 'description "Display a block and its references as a tree, up to given depth")
  (doc 'export #t)
  (let ([hash (find-block-by-prefix fs hash-prefix)])
       (if (not hash)
           (display (format "No block found with prefix: ~a\n" hash-prefix))
           (begin
            (display "╔══════════════════════════════════════════════════════════════╗\n")
            (display "║                     BLOCK TREE VIEW                          ║\n")
            (display "╚══════════════════════════════════════════════════════════════╝\n")
            (newline)
            (display-tree fs hash "" max-depth 0 (make-eq-hashtable))
            (newline)))))

(define (display-tree fs hash indent max-depth current-depth visited)
  (doc 'type (-> FS Bytevector String Nat Nat Hashtable Void))
  (doc 'description "Recursively display a block tree with indentation")
  (let ([hash-hex (hash->hex hash)])
       ;; Check if we've already visited this block (cycle detection)
       (if (hashtable-ref visited hash #f)
           (display (format "~a~a [already shown]\n" indent (short-hash hash-hex)))
           (begin
            (hashtable-set! visited hash #t)

            (let ([blk (fs-fetch fs hash)])
                 (if (not blk)
                     (display (format "~a~a [missing]\n" indent (short-hash hash-hex)))
                     (begin
                      (display (format "~a~a (~a, ~a bytes, ~a refs)\n"
                                       indent
                                       (short-hash hash-hex)
                                       (block-tag blk)
                                       (bytevector-length (block-payload blk))
                                       (vector-length (block-refs blk))))

                      ;; Recurse into refs if not at max depth
                      (when (< current-depth max-depth)
                            (let ([refs (block-refs blk)])
                                 (let loop ([i 0])
                                      (when (< i (vector-length refs))
                                            (let* ([is-last (= i (- (vector-length refs) 1))]
                                                   [new-indent (string-append indent
                                                                              (if is-last "  └─ " "  ├─ "))])
                                                  (display-tree fs
                                                                (vector-ref refs i)
                                                                new-indent
                                                                max-depth
                                                                (+ current-depth 1)
                                                                visited)
                                                  (loop (+ i 1))))))))))))))

(define (short-hash hash-hex)
  (doc 'type (-> String String))
  (doc 'description "Show just the first 8 characters of a hash")
  (substring hash-hex 0 (min 8 (string-length hash-hex))))

(doc 'section 'analytics)

(define (block-stats fs)
  (doc 'type (-> FS Void))
  (doc 'description "Display comprehensive statistics about the block store")
  (doc 'export #t)
  (let* ([all-hashes (fs-all-hashes fs)]
         [total-blocks (length all-hashes)]
         [tag-counts (make-eq-hashtable)]
         [total-payload-bytes 0]
         [total-refs 0]
         [ref-counts (make-hashtable equal-hash equal?)])

        ;; Gather statistics
        (for-each
         (lambda (hash)
                 ;; Guard against corrupted blocks - skip them instead of crashing
                 (guard (e [else (void)])  ; Skip corrupted blocks silently
                        (let ([blk (fs-fetch fs hash)])
                             (when blk
                                   ;; Count by tag
                                   (let* ([tag (block-tag blk)]
                                          [current (hashtable-ref tag-counts tag 0)])
                                         (hashtable-set! tag-counts tag (+ current 1)))

                                   ;; Accumulate payload size
                                   (set! total-payload-bytes
                                         (+ total-payload-bytes (bytevector-length (block-payload blk))))

                                   ;; Count refs and track inbound references
                                   (let ([refs (block-refs blk)])
                                        (set! total-refs (+ total-refs (vector-length refs)))
                                        (let loop ([i 0])
                                             (when (< i (vector-length refs))
                                                   (let* ([ref-hash (vector-ref refs i)]
                                                          [current (hashtable-ref ref-counts ref-hash 0)])
                                                         (hashtable-set! ref-counts ref-hash (+ current 1))
                                                         (loop (+ i 1))))))))))
         all-hashes)

        ;; Display statistics
        (display "╔══════════════════════════════════════════════════════════════╗\n")
        (display "║                  BLOCK STORE STATISTICS                      ║\n")
        (display "╚══════════════════════════════════════════════════════════════╝\n")
        (newline)
        (display (format "Total blocks:    ~a\n" total-blocks))
        (display (format "Total payload:   ~a bytes (~a KB)\n"
                         total-payload-bytes
                         (quotient total-payload-bytes 1024)))
        (display (format "Total refs:      ~a\n" total-refs))
        (display (format "Avg refs/block:  ~a\n"
                         (if (> total-blocks 0)
                             (inexact (/ total-refs total-blocks))
                             0)))
        (newline)

        ;; Tag distribution
        (display "Block types:\n")
        (let-values ([(tag-vec count-vec) (hashtable-entries tag-counts)])
                    (let ([tags (vector->list tag-vec)]
                          [counts (vector->list count-vec)])
                         (for-each
                          (lambda (tag count)
                                  (display (format "  ~a: ~a (~a%)\n"
                                                   tag
                                                   count
                                                   (inexact (/ (* count 100) total-blocks)))))
                          tags
                          counts)))
        (newline)

        ;; Reference analysis
        (let-values ([(ref-hash-vec ref-count-vec) (hashtable-entries ref-counts)])
                    (let* ([ref-count-vals (vector->list ref-count-vec)]
                           [max-refs (if (null? ref-count-vals) 0 (apply max ref-count-vals))]
                           [orphan-count (- total-blocks (hashtable-size ref-counts))])
                          (display (format "Most referenced: ~a inbound refs\n" max-refs))
                          (display (format "Orphan blocks:   ~a (no inbound refs)\n" orphan-count))))
        (newline)))

(define (find-popular fs n)
  (doc 'type (-> FS Nat Void))
  (doc 'description "Find the n most-referenced blocks")
  (doc 'export #t)
  (let* ([all-hashes (fs-all-hashes fs)]
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

        ;; Convert to list and sort
        (let-values ([(hash-vec count-vec) (hashtable-entries ref-counts)])
                    (let* ([hashes (vector->list hash-vec)]
                           [counts (vector->list count-vec)]
                           [pairs (map cons hashes counts)]
                           [sorted (list-sort (lambda (a b) (> (cdr a) (cdr b))) pairs)]
                           ;; Filter out dangling refs (hashes not in store)
                           [existing (filter (lambda (p) (fs-fetch fs (car p))) sorted)]
                           [top-n (take (min n (length existing)) existing)])

                          (display "╔══════════════════════════════════════════════════════════════╗\n")
                          (display "║                   MOST POPULAR BLOCKS                        ║\n")
                          (display "╚══════════════════════════════════════════════════════════════╝\n")
                          (newline)
                          (for-each
                           (lambda (pair)
                                   (let* ([hash (car pair)]
                                          [count (cdr pair)]
                                          [blk (fs-fetch fs hash)])
                                         (display (format "~a refs | ~a | ~a\n"
                                                          count
                                                          (short-hash (hash->hex hash))
                                                          (if blk (block-tag blk) "[missing]")))))
                           top-n)
                          (newline)))))

(define (find-orphans fs)
  (doc 'type (-> FS Void))
  (doc 'description "Find blocks with no inbound references")
  (doc 'export #t)
  (let* ([all-hashes (fs-all-hashes fs)]
         [referenced (make-hashtable equal-hash equal?)])

        ;; Mark all referenced blocks
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

        ;; Find orphans (blocks not in referenced set)
        (let ([orphans (filter (lambda (hash) (not (hashtable-ref referenced hash #f)))
                               all-hashes)])
             (display "╔══════════════════════════════════════════════════════════════╗\n")
             (display "║                      ORPHAN BLOCKS                           ║\n")
             (display "╚══════════════════════════════════════════════════════════════╝\n")
             (newline)
             (display (format "Found ~a orphan blocks (no inbound refs)\n\n" (length orphans)))
             (for-each
              (lambda (hash)
                      (let ([blk (fs-fetch fs hash)])
                           (display (format "~a | ~a | ~a bytes\n"
                                            (short-hash (hash->hex hash))
                                            (if blk (block-tag blk) "[missing]")
                                            (if blk (bytevector-length (block-payload blk)) 0)))))
              orphans)
             (newline))))

(doc 'section 'search)

(define *search-default-limit* 50)

(define (search-ranked fs query . args)
  (doc 'type (-> FS String Void))
  (doc 'description "Search for blocks containing query string, ranked by relevance. Optional limit arg (default 50, #f for all).")
  (doc 'export #t)
  (let* ([limit (if (null? args) *search-default-limit* (car args))]
         [all-hashes (fs-all-hashes fs)]
         [results '()])

        ;; Search all blocks
        (for-each
         (lambda (hash)
                 (let ([blk (fs-fetch fs hash)])
                      (when blk
                            (let* ([tag-str (symbol->string (block-tag blk))]
                                   [payload-str (guard (e [else ""])
                                                       (utf8->string (block-payload blk)))]
                                   [tag-score (if (bn-string-contains-ci? tag-str query) 10 0)]
                                   [payload-score (bn-count-occurrences payload-str query)]
                                   [total-score (+ tag-score payload-score)])
                                  (when (> total-score 0)
                                        (set! results (cons (list hash blk total-score) results)))))))
         all-hashes)

        ;; Sort by score descending
        (let* ([sorted (list-sort (lambda (a b) (> (caddr a) (caddr b))) results)]
               [total (length sorted)]
               [display-list (if (and limit (> total limit))
                                 (take limit sorted)
                                 sorted)])
             (display "╔══════════════════════════════════════════════════════════════╗\n")
             (display "║                      SEARCH RESULTS                          ║\n")
             (display "╚══════════════════════════════════════════════════════════════╝\n")
             (newline)
             (display (format "Query: \"~a\"\n" query))
             (if (and limit (> total limit))
                 (display (format "Found: ~a blocks (showing top ~a)\n\n" total limit))
                 (display (format "Found: ~a blocks\n\n" total)))

             (for-each
              (lambda (result)
                      (let* ([hash (car result)]
                             [blk (cadr result)]
                             [score (caddr result)]
                             [payload-preview (guard (e [else "[binary]"])
                                                     (truncate-string (utf8->string (block-payload blk)) 60))])
                            (display (format "[~a] ~a | ~a\n"
                                             score
                                             (short-hash (hash->hex hash))
                                             (block-tag blk)))
                            (display (format "     ~a\n" payload-preview))
                            (newline)))
              display-list))))

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

(define (bn-count-occurrences haystack needle)
  (doc 'type (-> String String Nat))
  (doc 'description "Count how many times needle appears in haystack (case-insensitive)")
  (let ([hay-lower (string-downcase haystack)]
        [need-lower (string-downcase needle)]
        [need-len (string-length needle)])
       (let loop ([i 0] [count 0])
            (cond
             [(> (+ i need-len) (string-length hay-lower)) count]
             [(string=? (substring hay-lower i (+ i need-len)) need-lower)
              (loop (+ i need-len) (+ count 1))]
             [else (loop (+ i 1) count)]))))

(doc 'section 'lineage)

(define (show-lineage fs hash-prefix)
  (doc 'type (-> FS String Void))
  (doc 'description "Show the full reference chain starting from a block")
  (doc 'export #t)
  (let ([hash (find-block-by-prefix fs hash-prefix)])
       (if (not hash)
           (display (format "No block found with prefix: ~a\n" hash-prefix))
           (begin
            (display "╔══════════════════════════════════════════════════════════════╗\n")
            (display "║                     BLOCK LINEAGE                            ║\n")
            (display "╚══════════════════════════════════════════════════════════════╝\n")
            (newline)
            (display-lineage fs hash 0 (make-eq-hashtable))
            (newline)))))

(define (display-lineage fs hash depth visited)
  (doc 'type (-> FS Bytevector Nat Hashtable Void))
  (doc 'description "Display lineage recursively")
  (if (hashtable-ref visited hash #f)
      (display (format "~a[cycle detected]\n" (make-string (* depth 2) #\space)))
      (begin
       (hashtable-set! visited hash #t)
       (let ([blk (fs-fetch fs hash)])
            (if (not blk)
                (display (format "~a[missing block]\n" (make-string (* depth 2) #\space)))
                (begin
                 (display (format "~a~a (~a)\n"
                                  (make-string (* depth 2) #\space)
                                  (short-hash (hash->hex hash))
                                  (block-tag blk)))
                 (let ([refs (block-refs blk)])
                      (when (> (vector-length refs) 0)
                            (display-lineage fs (vector-ref refs 0) (+ depth 1) visited)))))))))

(doc 'note "take is provided by core/prelude.ss as (take n lst)")

(doc 'section 'exports)
(doc 'note "This file provides the following public functions:")
(doc 'note "explore, describe-block, visualize-tree, block-stats")
(doc 'note "find-popular, find-orphans, search-ranked, show-lineage")
(doc 'note "Load this after boundary/io/fs.ss and core/block.ss are available")
