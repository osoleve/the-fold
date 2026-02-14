(source-directories (cons "core" (source-directories)))
(source-directories (cons "shell" (source-directories)))

(load "base/prelude.ss")
(load "blocks/block.ss")
(load "blocks/cas.ss")
(load "boundary/storage/cas-persist.ss")

(doc 'module 'store-analyze)
(doc 'description "Content-Addressed Store Analyzer")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(core/base/prelude core/blocks/block core/blocks/cas boundary/storage/cas-persist))

(doc 'note "Analyze the block store for usage patterns, statistics, and health. Helps understand storage growth and optimize the store")

(doc 'features "
  - Compute store statistics (size, block count, etc.)
  - Analyze block size distribution
  - Find duplicate blocks (hash collisions would be here)
  - Identify orphaned blocks
  - Compute storage efficiency metrics
  - Track reference patterns
  - Generate growth projections")

(doc 'usage "
  (store-stats fs)
  (store-distribution fs)
  (store-health-check fs)
  (store-report fs \"report.txt\")
  (store-growth-analysis fs)")

(doc 'section 'store-statistics)

(define (compute-store-stats fs)
  (doc 'type (-> FS Stats))
  (doc 'description "Compute comprehensive statistics about the store")
  (doc 'returns "((total-blocks . N) (total-bytes . N) (avg-block-size . N) ...)")
  (doc 'export #t)
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
                     (let ([avg-size (quotient total-bytes total-blocks)])
                          `((total-blocks . ,total-blocks)
                            (total-bytes . ,total-bytes)
                            (avg-block-size . ,avg-size)
                            (min-block-size . ,(or min-size 0))
                            (max-block-size . ,max-size)
                            (tag-distribution . ,tag-counts)
                            (ref-distribution . ,ref-counts)))

                     (let* ([hash (car hashes)]
                            [result (fetch store hash)])
                           (if (error? result)
                               (loop (cdr hashes) total-bytes min-size max-size tag-counts ref-counts)
                               (let* ([block (result-value result)]
                                      [payload-size (bytevector-length (block-payload block))]
                                      [block-size (+ payload-size (* (vector-length (block-refs block)) address-size))]
                                      [tag (block-tag block)]
                                      [num-refs (vector-length (block-refs block))])

                                     (let ([current-count (hashtable-ref tag-counts tag 0)])
                                          (hashtable-set! tag-counts tag (+ current-count 1)))

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

(doc 'section 'display-statistics)

(define (store-stats fs)
  (doc 'type (-> FS Void))
  (doc 'description "Display store statistics")
  (doc 'export #t)
  (display "\n============== STORE STATISTICS ================================\n\n")

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

(define (hashtable-map ht fn)
  (doc 'type (-> Hashtable (-> Key Value α) (List α)))
  (doc 'description "Map a function over hashtable entries")
  (let ([keys (vector->list (hashtable-keys ht))])
       (map (lambda (k)
                    (fn k (hashtable-ref ht k #f)))
            keys)))

;; Note: list-sort is a Chez Scheme built-in (O(n log n) merge sort)

(doc 'section 'size-distribution-analysis)

(define (store-distribution fs)
  (doc 'type (-> FS Void))
  (doc 'description "Show block size distribution with histogram")
  (doc 'export #t)
  (display "\n============ BLOCK SIZE DISTRIBUTION ===========================\n\n")

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

(define (make-buckets sizes)
  (doc 'type (-> (List Nat) (List (Pair String Nat))))
  (doc 'description "Create histogram buckets from sizes")
  (let ([ranges '((0 . "0-100")
                  (101 . "101-500")
                  (501 . "501-1K")
                  (1001 . "1K-5K")
                  (5001 . "5K-10K")
                  (10001 . "10K+"))]
        [counts (make-vector 6 0)])

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

       (map (lambda (i)
                    (cons (cdr (list-ref ranges i))
                          (vector-ref counts i)))
            '(0 1 2 3 4 5))))

(define (make-bar count width max-count)
  (doc 'type (-> Nat Nat Nat String))
  (doc 'description "Create a bar for histogram display")
  (doc 'param 'count "The value")
  (doc 'param 'width "Max characters")
  (doc 'param 'max-count "For scaling")
  (if (= max-count 0)
      ""
      (let ([bar-length (quotient (* count width) max-count)])
           (make-string bar-length #\█))))

(define (make-string n c)
  (doc 'type (-> Nat Char String))
  (doc 'description "Create string of n copies of character")
  (list->string (make-list-of n c)))

(define (make-list-of n x)
  (doc 'type (-> Nat α (List α)))
  (doc 'description "Create list of n copies of x")
  (if (= n 0)
      '()
      (cons x (make-list-of (- n 1) x))))

(doc 'section 'store-health-check)

(define (store-health-check fs)
  (doc 'type (-> FS Void))
  (doc 'description "Check store health and integrity")
  (doc 'export #t)
  (display "\n============= STORE HEALTH CHECK ===============================\n\n")

  (let* ([store (fs-make-cas fs)]
         [all-hashes (cas-all-hashes store)]
         [total (length all-hashes)])

        (display (format "Checking ~a blocks...\n\n" total))

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

        (display "3. Finding orphaned blocks...\n")
        (let ([orphans (find-orphaned-blocks store all-hashes)])
             (display (format "   Found ~a orphaned blocks (~a%)\n"
                              (length orphans)
                              (if (= total 0) 0
                                  (exact (round (* 100 (/ (length orphans) total))))))))

        (display "\n")
        (display "Health check complete.\n")))

(define (find-broken-refs store all-hashes)
  (doc 'type (-> CAS (List Hash) (List (Pair Hash Hash))))
  (doc 'description "Find references to non-existent blocks")
  (doc 'returns "List of (block-hash . missing-ref-hash) pairs")
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

(define (find-orphaned-blocks store all-hashes)
  (doc 'type (-> CAS (List Hash) (List Hash)))
  (doc 'description "Find blocks that are not referenced by any other block")
  (let ([referenced (make-hash-set '())])
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

       (filter (lambda (hash)
                       (not (hash-set-contains? referenced hash)))
               all-hashes)))

(doc 'section 'hash-set)

(define (make-hash-set hashes)
  (doc 'type (-> (List Hash) HashSet))
  (doc 'description "Create a hash set from a list of hashes")
  (let ([ht (make-hashtable bytevector-hash bytevector=?)])
       (for-each
        (lambda (hash)
                (hashtable-set! ht hash #t))
        hashes)
       ht))

(define (hash-set-contains? hs hash)
  (doc 'type (-> HashSet Hash Boolean))
  (doc 'description "Check if hash is in the set")
  (hashtable-contains? hs hash))

(define (hash-set-add! hs hash)
  (doc 'type (-> HashSet Hash Void))
  (doc 'description "Add hash to the set")
  (hashtable-set! hs hash #t))

(define (bytevector-hash bv)
  (doc 'type (-> Bytevector Nat))
  (doc 'description "Hash function for bytevectors")
  (let ([len (bytevector-length bv)])
       (let loop ([i 0] [hash 0])
            (if (= i len)
                hash
                (loop (+ i 1)
                      (+ (* hash 31) (bytevector-u8-ref bv i)))))))

(doc 'section 'store-report-generation)

(define (store-report fs output-file)
  (doc 'type (-> FS String Void))
  (doc 'description "Generate comprehensive store report")
  (doc 'export #t)
  (let ([stats (compute-store-stats fs)]
        [store (fs-make-cas fs)]
        [all-hashes (cas-all-hashes (fs-make-cas fs))])

       (call-with-output-file output-file
                              (lambda (port)
                                      (display "CONTENT-ADDRESSED STORE ANALYSIS REPORT\n" port)
                                      (display "====\n\n" port)

                                      (display "STORAGE STATISTICS\n" port)
                                      (display "----\n" port)
                                      (display (format "Total Blocks: ~a\n" (cdr (assq 'total-blocks stats))) port)
                                      (display (format "Total Storage: ~a bytes\n" (cdr (assq 'total-bytes stats))) port)
                                      (display (format "Average Block Size: ~a bytes\n" (cdr (assq 'avg-block-size stats))) port)
                                      (display "\n" port)

                                      (display "TAG DISTRIBUTION\n" port)
                                      (display "----\n" port)
                                      (let ([tag-dist (cdr (assq 'tag-distribution stats))])
                                           (hashtable-for-each tag-dist
                                                               (lambda (tag count)
                                                                       (display (format "~a: ~a\n" tag count) port))))
                                      (display "\n" port)

                                      (display "REFERENCE DISTRIBUTION\n" port)
                                      (display "----\n" port)
                                      (for-each
                                       (lambda (entry)
                                               (display (format "~a refs: ~a blocks\n" (car entry) (cdr entry)) port))
                                       (cdr (assq 'ref-distribution stats)))
                                      (display "\n" port)))

       (display (format "Report written to ~a\n" output-file))))

(define (hashtable-for-each ht proc)
  (doc 'type (-> Hashtable (-> Key Value Void) Void))
  (doc 'description "Iterate over hashtable entries")
  (let ([keys (vector->list (hashtable-keys ht))])
       (for-each
        (lambda (key)
                (proc key (hashtable-ref ht key #f)))
        keys)))

(doc 'section 'growth-analysis)

(define (store-growth-analysis fs)
  (doc 'type (-> FS Void))
  (doc 'description "Analyze store growth patterns")
  (doc 'note "Requires historical data")
  (doc 'export #t)
  (display "\n============ STORE GROWTH ANALYSIS =============================\n\n")

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

(doc 'section 'helper-functions)

(define (hash->hex hash)
  (doc 'type (-> Hash String))
  (doc 'description "Convert hash to hex string")
  (let* ([len (bytevector-length hash)]
         [hex-chars (make-string (* len 2) #\0)])
        (let loop ([i 0])
             (when (< i len)
                   (let ([byte (bytevector-u8-ref hash i)])
                        (string-set! hex-chars (* i 2) (int->hex-digit (quotient byte 16)))
                        (string-set! hex-chars (+ (* i 2) 1) (int->hex-digit (remainder byte 16)))
                        (loop (+ i 1)))))
        hex-chars))

(define (int->hex-digit n)
  (doc 'type (-> Nat Char))
  (doc 'description "Convert integer to hex digit character")
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
