;;; shell/blocks/block-navigator.ss — Interactive Block Store Navigator & Analytics
;;;
;;; Created by Secret-Builder (sonnet)
;;;
;;; A comprehensive tool for exploring and analyzing the content-addressed
;;; block store. Provides navigation, visualization, analytics, and search
;;; capabilities to understand the structure and relationships within The Fold.
;;;
;;; This is Shell code: uses filesystem capabilities, pretty-printing, stats.
;;;
;;; Features:
;;;   - Interactive block exploration (drill down by hash)
;;;   - Relationship visualization (tree and graph views)
;;;   - Analytics (block type distribution, size stats, reference counts)
;;;   - Enhanced search with relevance ranking
;;;   - Orphan detection (blocks with no inbound refs)
;;;   - Most referenced blocks (popularity analysis)
;;;   - Block lineage tracking (full ref chain)
;;;
;;; Usage:
;;;   (load "shell/block-navigator.ss")
;;;   (explore (fs) hash-prefix)      ; Explore a block and its refs
;;;   (block-stats (fs))               ; Show store statistics
;;;   (find-popular (fs) n)            ; Find n most-referenced blocks
;;;   (find-orphans (fs))              ; Find blocks with no inbound refs
;;;   (visualize-tree (fs) hash depth); Show block tree
;;;   (search-ranked (fs) query)       ; Search with ranking

;;; ====
;;; Dependencies (must be loaded first)
;;; ====
;;;   core/block.ss
;;;   core/sha256.ss
;;;   shell/fs.ss

;;; ====
;;; Block Information Display
;;; ====

;;; describe-block : FS × Bytevector → void
;;; Display detailed information about a single block.
(define (describe-block fs hash)
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

;;; truncate-string : String × Nat → String
(define (truncate-string str max-len)
  (if (> (string-length str) max-len)
      (string-append (substring str 0 max-len) "...")
      str))

;;; ====
;;; Interactive Exploration
;;; ====

;;; explore : FS × String → void
;;; Explore a block by hash prefix, showing it and its immediate refs.
(define (explore fs hash-prefix)
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

;;; explore-ref : FS × String × Nat → void
;;; Explore the nth reference of a block.
(define (explore-ref fs hash-prefix n)
  (let ([hash (find-block-by-prefix fs hash-prefix)])
       (if (not hash)
           (display (format "No block found with prefix: ~a\n" hash-prefix))
           (let ([blk (fs-fetch fs hash)])
                (let ([refs (block-refs blk)])
                     (if (>= n (vector-length refs))
                         (display (format "Reference index ~a out of bounds (max: ~a)\n"
                                          n (- (vector-length refs) 1)))
                         (explore fs (hash->hex (vector-ref refs n)))))))))

;;; find-block-by-prefix : FS × String → Bytevector | #f
;;; Find a block hash that starts with the given prefix.
(define (find-block-by-prefix fs prefix)
  (let ([all-hashes (fs-all-hashes fs)])
       (let loop ([hashes all-hashes])
            (if (null? hashes)
                #f
                (let ([hash (car hashes)])
                     (if (string-prefix? prefix (hash->hex hash))
                         hash
                         (loop (cdr hashes))))))))

;;; string-prefix? is now provided by shell/fs.ss

;;; ====
;;; Tree Visualization
;;; ====

;;; visualize-tree : FS × String × Nat → void
;;; Display a block and its references as a tree, up to given depth.
(define (visualize-tree fs hash-prefix max-depth)
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

;;; display-tree : FS × Bytevector × String × Nat × Nat × Hashtable → void
;;; Recursively display a block tree with indentation.
(define (display-tree fs hash indent max-depth current-depth visited)
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

;;; short-hash : String → String
;;; Show just the first 8 characters of a hash.
(define (short-hash hash-hex)
  (substring hash-hex 0 (min 8 (string-length hash-hex))))

;;; ====
;;; Analytics & Statistics
;;; ====

;;; block-stats : FS → void
;;; Display comprehensive statistics about the block store.
(define (block-stats fs)
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

;;; find-popular : FS × Nat → void
;;; Find the n most-referenced blocks.
(define (find-popular fs n)
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
                           [top-n (take (min n (length sorted)) sorted)])
                          
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

;;; find-orphans : FS → void
;;; Find blocks with no inbound references.
(define (find-orphans fs)
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

;;; ====
;;; Enhanced Search
;;; ====

;;; search-ranked : FS × String → void
;;; Search for blocks containing query string, ranked by relevance.
(define (search-ranked fs query)
  (let* ([all-hashes (fs-all-hashes fs)]
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
        (let ([sorted (list-sort (lambda (a b) (> (caddr a) (caddr b))) results)])
             (display "╔══════════════════════════════════════════════════════════════╗\n")
             (display "║                      SEARCH RESULTS                          ║\n")
             (display "╚══════════════════════════════════════════════════════════════╝\n")
             (newline)
             (display (format "Query: \"~a\"\n" query))
             (display (format "Found: ~a blocks\n\n" (length sorted)))
             
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
              sorted))))

;;; bn-string-contains-ci? : String × String → Boolean
;;; Case-insensitive substring search.
(define (bn-string-contains-ci? haystack needle)
  (let ([hay-lower (string-downcase haystack)]
        [need-lower (string-downcase needle)])
       (bn-string-contains? hay-lower need-lower)))

;;; bn-string-contains? : String × String → Boolean
(define (bn-string-contains? haystack needle)
  (let ([need-len (string-length needle)])
       (let loop ([i 0])
            (cond
             [(> (+ i need-len) (string-length haystack)) #f]
             [(string=? (substring haystack i (+ i need-len)) needle) #t]
             [else (loop (+ i 1))]))))

;;; bn-count-occurrences : String × String → Nat
;;; Count how many times needle appears in haystack (case-insensitive).
(define (bn-count-occurrences haystack needle)
  (let ([hay-lower (string-downcase haystack)]
        [need-lower (string-downcase needle)]
        [need-len (string-length needle)])
       (let loop ([i 0] [count 0])
            (cond
             [(> (+ i need-len) (string-length hay-lower)) count]
             [(string=? (substring hay-lower i (+ i need-len)) need-lower)
              (loop (+ i need-len) (+ count 1))]
             [else (loop (+ i 1) count)]))))

;;; ====
;;; Lineage Tracking
;;; ====

;;; show-lineage : FS × String → void
;;; Show the full reference chain starting from a block.
(define (show-lineage fs hash-prefix)
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

;;; display-lineage : FS × Bytevector × Nat × Hashtable → void
(define (display-lineage fs hash depth visited)
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

;;; ====
;;; take is provided by core/prelude.ss as (take n lst)
;;; take is provided by core/prelude.ss as (take n lst)
;;; take is provided by core/prelude.ss as (take n lst)
;;; take is provided by core/prelude.ss as (take n lst)
;;; take is provided by core/prelude.ss as (take n lst)
;;; take is provided by core/prelude.ss as (take n lst)
;;; take is provided by core/prelude.ss as (take n lst)

;;; ====
;;; Export Note
;;; ====

;;; This file provides the following public functions:
;;;   explore, describe-block, visualize-tree, block-stats
;;;   find-popular, find-orphans, search-ranked, show-lineage
;;;
;;; Load this after shell/fs.ss and core/block.ss are available.
