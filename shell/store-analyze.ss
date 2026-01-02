;;; thimble/store-analyze.ss — Content-Addressed Store Analyzer
;;;
;;; Analyze the block store for usage patterns, statistics, and health.
;;; Helps understand storage growth and optimize the store.
;;;
;;; Features:
;;;   - Compute store statistics (size, block count, etc.)
;;;   - Analyze block size distribution
;;;   - Find duplicate blocks (hash collisions would be here)
;;;   - Identify orphaned blocks
;;;   - Compute storage efficiency metrics
;;;   - Track reference patterns
;;;   - Generate growth projections
;;;
;;; This is Shell code: uses filesystem, aggregates data.
;;;
;;; Usage:
;;;   (store-stats fs)
;;;   (store-distribution fs)
;;;   (store-health-check fs)
;;;   (store-report fs "report.txt")
;;;   (store-growth-analysis fs)
;;;
;;; Dependencies:
;;;   core/block.ss
;;;   core/cas.ss
;;;   shell/fs.ss
;;;   shell/cas-persist.ss

;;; Set up source-directories to find modules
(source-directories (cons "core" (source-directories)))
(source-directories (cons "shell" (source-directories)))

(load "base/prelude.ss")
(load "blocks/block.ss")
(load "blocks/cas.ss")
(load "cas-persist.ss")

;;; ============================================================
;;; Store Statistics
;;; ============================================================

;;; compute-store-stats : FS → Stats
;;; Compute comprehensive statistics about the store.
;;; Returns: ((total-blocks . N) (total-bytes . N) (avg-block-size . N) ...)
(define (compute-store-stats fs)
  (let* ([store (fs-make-cas fs)]
         [all-hashes (cas-all-hashes store)]
         [total-blocks (length all-hashes)])
        
        (if (= total-blocks 0)
            `((total-blocks . 0)
              (total-bytes . 0)
              (avg-block-size . 0)
              (min-block-size . 0)
              (max-block-size . 0))
            (let loop ([hashes all-hashes]
                       [total-bytes 0]
                       [min-size #f]
                       [max-size 0]
                       [tag-counts (make-eq-hashtable)]
                       [ref-counts '()])
                 
                 (if (null? hashes)
                     ;; Compute final stats
                     (let ([avg-size (quotient total-bytes total-blocks)])
                          `((total-blocks . ,total-blocks)
                            (total-bytes . ,total-bytes)
                            (avg-block-size . ,avg-size)
                            (min-block-size . ,(or min-size 0))
                            (max-block-size . ,max-size)
                            (tag-distribution . ,tag-counts)
                            (ref-distribution . ,ref-counts)))
                     
                     ;; Process next block
                     (let* ([hash (car hashes)]
                            [result (fetch store hash)])
                           (if (error? result)
                               (loop (cdr hashes) total-bytes min-size max-size tag-counts ref-counts)
                               (let* ([block (result-value result)]
                                      [payload-size (bytevector-length (block-payload block))]
                                      [block-size (+ payload-size (* (vector-length (block-refs block)) address-size))]
                                      [tag (block-tag block)]
                                      [num-refs (vector-length (block-refs block))])
                                     
                                     ;; Update tag counts
                                     (let ([current-count (hashtable-ref tag-counts tag 0)])
                                          (hashtable-set! tag-counts tag (+ current-count 1)))
                                     
                                     ;; Update ref distribution
                                     (let ([ref-entry (assv num-refs ref-counts)])
                                          (if ref-entry
                                              (set-cdr! ref-entry (+ (cdr ref-entry) 1))
                                              (set! ref-counts (cons (cons num-refs 1) ref-counts))))
                                     
                                     (loop (cdr hashes)
                                           (+ total-bytes block-size)
                                           (if min-size (min min-size block-size) block-size)
                                           (max max-size block-size)
                                           tag-counts
                                           ref-counts)))))))))

;;; ============================================================
;;; Display Statistics
;;; ============================================================

;;; store-stats : FS → void
;;; Display store statistics.
(define (store-stats fs)
  (display "\n╔══════════════════════════════════════════════════════════════╗\n")
  (display "║                  STORE STATISTICS                            ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n\n")
  
  (let ([stats (compute-store-stats fs)])
       (let ([total-blocks (cdr (assq 'total-blocks stats))]
             [total-bytes (cdr (assq 'total-bytes stats))]
             [avg-size (cdr (assq 'avg-block-size stats))]
             [min-size (cdr (assq 'min-block-size stats))]
             [max-size (cdr (assq 'max-block-size stats))])
            
            (display (format "Total Blocks: ~a\n" total-blocks))
            (display (format "Total Storage: ~a bytes (~a KB)\n"
                             total-bytes
                             (quotient total-bytes 1024)))
            (display "\n")
            (display (format "Block Sizes:\n"))
            (display (format "  Average: ~a bytes\n" avg-size))
            (display (format "  Minimum: ~a bytes\n" min-size))
            (display (format "  Maximum: ~a bytes\n" max-size))
            (display "\n")
            
            ;; Tag distribution
            (let ([tag-dist (cdr (assq 'tag-distribution stats))])
                 (display "Tag Distribution:\n")
                 (let ([tags (list-sort
                              (lambda (a b) (> (cdr a) (cdr b)))
                              (hashtable-map tag-dist cons))])
                      (for-each
                       (lambda (entry)
                               (let ([tag (car entry)]
                                     [count (cdr entry)]
                                     [percent (exact (round (* 100 (/ count total-blocks))))])
                                    (display (format "  ~a: ~a (~a%)\n" tag count percent))))
                       tags)))
            (display "\n")
            
            ;; Reference distribution
            (let ([ref-dist (cdr (assq 'ref-distribution stats))])
                 (display "Reference Count Distribution:\n")
                 (let ([sorted (list-sort
                                (lambda (a b) (< (car a) (car b)))
                                ref-dist)])
                      (for-each
                       (lambda (entry)
                               (let ([num-refs (car entry)]
                                     [count (cdr entry)])
                                    (display (format "  ~a refs: ~a blocks\n" num-refs count))))
                       sorted))))))

;;; hashtable-map : Hashtable × (Key × Value → α) → (List α)
;;; Map a function over hashtable entries.
(define (hashtable-map ht fn)
  (let ([keys (vector->list (hashtable-keys ht))])
       (map (lambda (k)
                    (fn k (hashtable-ref ht k #f)))
            keys)))

;;; list-sort : (α × α → Boolean) × (List α) → (List α)
;;; Sort a list using the given comparison function.
(define (list-sort less? lst)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (let* ([pivot (car lst)]
             [rest (cdr lst)]
             [smaller (filter (lambda (x) (less? x pivot)) rest)]
             [greater (filter (lambda (x) (not (less? x pivot))) rest)])
            (append (list-sort less? smaller)
                    (list pivot)
                    (list-sort less? greater)))))

;;; ============================================================
;;; Size Distribution Analysis
;;; ============================================================

;;; store-distribution : FS → void
;;; Show block size distribution with histogram.
(define (store-distribution fs)
  (display "\n╔══════════════════════════════════════════════════════════════╗\n")
  (display "║              BLOCK SIZE DISTRIBUTION                         ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n\n")
  
  (let* ([store (fs-make-cas fs)]
         [all-hashes (cas-all-hashes store)]
         [sizes (map (lambda (hash)
                             (let ([result (fetch store hash)])
                                  (if (error? result)
                                      0
                                      (let ([block (result-value result)])
                                           (bytevector-length (block-payload block))))))
                     all-hashes)]
         [buckets (make-buckets sizes)])
        
        (display "Payload Size Distribution:\n\n")
        (for-each
         (lambda (bucket)
                 (let ([range (car bucket)]
                       [count (cdr bucket)])
                      (display (format "  ~a: ~a blocks " range count))
                      (display (make-bar count 50 (apply max (map cdr buckets))))
                      (display "\n")))
         buckets)))

;;; make-buckets : (List Nat) → (List (Pair String Nat))
;;; Create histogram buckets from sizes.
(define (make-buckets sizes)
  (let ([ranges '((0 . "0-100")
                  (101 . "101-500")
                  (501 . "501-1K")
                  (1001 . "1K-5K")
                  (5001 . "5K-10K")
                  (10001 . "10K+"))]
        [counts (make-vector 6 0)])
       
       ;; Count sizes into buckets
       (for-each
        (lambda (size)
                (cond
                 [(<= size 100) (vector-set! counts 0 (+ (vector-ref counts 0) 1))]
                 [(<= size 500) (vector-set! counts 1 (+ (vector-ref counts 1) 1))]
                 [(<= size 1000) (vector-set! counts 2 (+ (vector-ref counts 2) 1))]
                 [(<= size 5000) (vector-set! counts 3 (+ (vector-ref counts 3) 1))]
                 [(<= size 10000) (vector-set! counts 4 (+ (vector-ref counts 4) 1))]
                 [else (vector-set! counts 5 (+ (vector-ref counts 5) 1))]))
        sizes)
       
       ;; Build result
       (map (lambda (i)
                    (cons (cdr (list-ref ranges i))
                          (vector-ref counts i)))
            '(0 1 2 3 4 5))))

;;; make-bar : Nat × Nat × Nat → String
;;; Create a bar for histogram display.
;;; count is the value, width is max characters, max-count is for scaling.
(define (make-bar count width max-count)
  (if (= max-count 0)
      ""
      (let ([bar-length (quotient (* count width) max-count)])
           (make-string bar-length #\█))))

;;; make-string : Nat × Char → String
;;; Create string of n copies of character.
(define (make-string n c)
  (list->string (make-list-of n c)))

;;; make-list-of : Nat × α → (List α)
(define (make-list-of n x)
  (if (= n 0)
      '()
      (cons x (make-list-of (- n 1) x))))

;;; ============================================================
;;; Store Health Check
;;; ============================================================

;;; store-health-check : FS → void
;;; Check store health and integrity.
(define (store-health-check fs)
  (display "\n╔══════════════════════════════════════════════════════════════╗\n")
  (display "║                 STORE HEALTH CHECK                           ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n\n")
  
  (let* ([store (fs-make-cas fs)]
         [all-hashes (cas-all-hashes store)]
         [total (length all-hashes)])
        
        (display (format "Checking ~a blocks...\n\n" total))
        
        ;; Check 1: Verify all blocks are fetchable
        (display "1. Verifying block integrity...\n")
        (let ([unfetchable (filter (lambda (hash)
                                           (error? (fetch store hash)))
                                   all-hashes)])
             (if (null? unfetchable)
                 (display "   ✓ All blocks fetchable\n")
                 (begin
                  (display (format "   ✗ ~a blocks unfetchable\n" (length unfetchable)))
                  (for-each
                   (lambda (hash)
                           (display (format "     - ~a\n" (hash->hex hash))))
                   unfetchable))))
        
        (display "\n")
        
        ;; Check 2: Verify referenced blocks exist
        (display "2. Checking reference integrity...\n")
        (let ([broken-refs (find-broken-refs store all-hashes)])
             (if (null? broken-refs)
                 (display "   ✓ All referenced blocks exist\n")
                 (begin
                  (display (format "   ✗ ~a broken references found\n" (length broken-refs)))
                  (for-each
                   (lambda (broken)
                           (display (format "     Block ~a references missing ~a\n"
                                            (hash->hex (car broken))
                                            (hash->hex (cdr broken)))))
                   broken-refs))))
        
        (display "\n")
        
        ;; Check 3: Find orphaned blocks
        (display "3. Finding orphaned blocks...\n")
        (let ([orphans (find-orphaned-blocks store all-hashes)])
             (display (format "   Found ~a orphaned blocks (~a%)\n"
                              (length orphans)
                              (if (= total 0) 0
                                  (exact (round (* 100 (/ (length orphans) total))))))))
        
        (display "\n")
        (display "Health check complete.\n")))

;;; find-broken-refs : CAS × (List Hash) → (List (Pair Hash Hash))
;;; Find references to non-existent blocks.
;;; Returns list of (block-hash . missing-ref-hash) pairs.
(define (find-broken-refs store all-hashes)
  (let ([hash-set (make-hash-set all-hashes)])
       (let loop ([hashes all-hashes]
                  [broken '()])
            (if (null? hashes)
                broken
                (let* ([hash (car hashes)]
                       [result (fetch store hash)])
                      (if (error? result)
                          (loop (cdr hashes) broken)
                          (let* ([block (result-value result)]
                                 [refs (block-refs block)]
                                 [missing (filter (lambda (ref)
                                                          (not (hash-set-contains? hash-set ref)))
                                                  (vector->list refs))])
                                (loop (cdr hashes)
                                      (append (map (lambda (ref) (cons hash ref)) missing)
                                              broken)))))))))

;;; find-orphaned-blocks : CAS × (List Hash) → (List Hash)
;;; Find blocks that are not referenced by any other block.
(define (find-orphaned-blocks store all-hashes)
  (let ([referenced (make-hash-set '())])
       ;; Collect all referenced hashes
       (for-each
        (lambda (hash)
                (let ([result (fetch store hash)])
                     (unless (error? result)
                             (let* ([block (result-value result)]
                                    [refs (block-refs block)])
                                   (vector-for-each
                                    (lambda (ref)
                                            (hash-set-add! referenced ref))
                                    refs)))))
        all-hashes)
       
       ;; Find unreferenced blocks
       (filter (lambda (hash)
                       (not (hash-set-contains? referenced hash)))
               all-hashes)))

;;; ============================================================
;;; Hash Set (for efficient membership testing)
;;; ============================================================

;;; make-hash-set : (List Hash) → HashSet
;;; Create a hash set from a list of hashes.
(define (make-hash-set hashes)
  (let ([ht (make-hashtable bytevector-hash bytevector=?)])
       (for-each
        (lambda (hash)
                (hashtable-set! ht hash #t))
        hashes)
       ht))

;;; hash-set-contains? : HashSet × Hash → Boolean
;;; Check if hash is in the set.
(define (hash-set-contains? hs hash)
  (hashtable-contains? hs hash))

;;; hash-set-add! : HashSet × Hash → void
;;; Add hash to the set.
(define (hash-set-add! hs hash)
  (hashtable-set! hs hash #t))

;;; bytevector-hash : Bytevector → Nat
;;; Hash function for bytevectors.
(define (bytevector-hash bv)
  (let ([len (bytevector-length bv)])
       (let loop ([i 0] [hash 0])
            (if (= i len)
                hash
                (loop (+ i 1)
                      (+ (* hash 31) (bytevector-u8-ref bv i)))))))

;;; ============================================================
;;; Store Report Generation
;;; ============================================================

;;; store-report : FS × String → void
;;; Generate comprehensive store report.
(define (store-report fs output-file)
  (let ([stats (compute-store-stats fs)]
        [store (fs-make-cas fs)]
        [all-hashes (cas-all-hashes (fs-make-cas fs))])
       
       (call-with-output-file output-file
                              (lambda (port)
                                      (display "CONTENT-ADDRESSED STORE ANALYSIS REPORT\n" port)
                                      (display "========================================\n\n" port)
                                      
                                      ;; Basic stats
                                      (display "STORAGE STATISTICS\n" port)
                                      (display "------------------\n" port)
                                      (display (format "Total Blocks: ~a\n" (cdr (assq 'total-blocks stats))) port)
                                      (display (format "Total Storage: ~a bytes\n" (cdr (assq 'total-bytes stats))) port)
                                      (display (format "Average Block Size: ~a bytes\n" (cdr (assq 'avg-block-size stats))) port)
                                      (display "\n" port)
                                      
                                      ;; Tag distribution
                                      (display "TAG DISTRIBUTION\n" port)
                                      (display "----------------\n" port)
                                      (let ([tag-dist (cdr (assq 'tag-distribution stats))])
                                           (hashtable-for-each tag-dist
                                                               (lambda (tag count)
                                                                       (display (format "~a: ~a\n" tag count) port))))
                                      (display "\n" port)
                                      
                                      ;; Reference distribution
                                      (display "REFERENCE DISTRIBUTION\n" port)
                                      (display "----------------------\n" port)
                                      (for-each
                                       (lambda (entry)
                                               (display (format "~a refs: ~a blocks\n" (car entry) (cdr entry)) port))
                                       (cdr (assq 'ref-distribution stats)))
                                      (display "\n" port)))
       
       (display (format "Report written to ~a\n" output-file))))

;;; hashtable-for-each : Hashtable × (Key × Value → void) → void
;;; Iterate over hashtable entries.
(define (hashtable-for-each ht proc)
  (let ([keys (vector->list (hashtable-keys ht))])
       (for-each
        (lambda (key)
                (proc key (hashtable-ref ht key #f)))
        keys)))

;;; ============================================================
;;; Growth Analysis
;;; ============================================================

;;; store-growth-analysis : FS → void
;;; Analyze store growth patterns (requires historical data).
(define (store-growth-analysis fs)
  (display "\n╔══════════════════════════════════════════════════════════════╗\n")
  (display "║              STORE GROWTH ANALYSIS                           ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n\n")
  
  (let ([stats (compute-store-stats fs)])
       (let ([total-blocks (cdr (assq 'total-blocks stats))]
             [total-bytes (cdr (assq 'total-bytes stats))]
             [avg-size (cdr (assq 'avg-block-size stats))])
            
            (display "Current State:\n")
            (display (format "  Blocks: ~a\n" total-blocks))
            (display (format "  Storage: ~a KB\n" (quotient total-bytes 1024)))
            (display "\n")
            
            (display "Growth Projections:\n")
            (display "  (Assumes linear growth at current rate)\n\n")
            
            ;; Project growth
            (let ([growth-rates '(10 100 1000 10000)])
                 (for-each
                  (lambda (new-blocks)
                          (let ([new-total (+ total-blocks new-blocks)]
                                [new-bytes (+ total-bytes (* new-blocks avg-size))])
                               (display (format "  After +~a blocks:\n" new-blocks))
                               (display (format "    Total: ~a blocks\n" new-total))
                               (display (format "    Storage: ~a KB (~a MB)\n"
                                                (quotient new-bytes 1024)
                                                (quotient new-bytes (* 1024 1024))))
                               (display "\n")))
                  growth-rates)))))

;;; ============================================================
;;; Helper Functions
;;; ============================================================

;;; hash->hex : Hash → String
;;; Convert hash to hex string (simplified version).
(define (hash->hex hash)
  (let* ([len (bytevector-length hash)]
         [hex-chars (make-string (* len 2) #\0)])
        (let loop ([i 0])
             (when (< i len)
                   (let ([byte (bytevector-u8-ref hash i)])
                        (string-set! hex-chars (* i 2) (int->hex-digit (quotient byte 16)))
                        (string-set! hex-chars (+ (* i 2) 1) (int->hex-digit (remainder byte 16)))
                        (loop (+ i 1)))))
        hex-chars))

;;; int->hex-digit : Nat → Char
(define (int->hex-digit n)
  (if (< n 10)
      (integer->char (+ (char->integer #\0) n))
      (integer->char (+ (char->integer #\a) (- n 10)))))

(display "Store analyzer loaded.\n")
(display "Usage:\n")
(display "  (store-stats fs)\n")
(display "  (store-distribution fs)\n")
(display "  (store-health-check fs)\n")
(display "  (store-report fs \"report.txt\")\n")
(display "  (store-growth-analysis fs)\n")
