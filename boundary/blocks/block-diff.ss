;;; Set up source-directories to find modules
(source-directories (cons "core" (source-directories)))
(source-directories (cons "boundary/storage" (source-directories)))

(load "base/prelude.ss")
(load "blocks/block.ss")
(load "blocks/cas.ss")
(load "blocks/normalize.ss")
(load "blocks/expand.ss")
(load "cas-persist.ss")

(doc 'module 'block-diff)
(doc 'description "Block Comparison and Diff Utility")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Compare two blocks structurally and show differences")
(doc 'note "Useful for understanding how blocks evolved or differ")
(doc 'dependencies '(core/block core/cas core/normalize core/expand boundary/cas-persist))

(doc 'section 'features)
(doc 'note "Structural comparison of blocks")
(doc 'note "Diff of normalized forms (ignore variable names)")
(doc 'note "Diff of expanded forms (show actual differences)")
(doc 'note "Side-by-side comparison")
(doc 'note "Similarity metrics")
(doc 'note "This is Shell code: uses filesystem, handles display")

(doc 'section 'comparison)

(define (block-equal? b1 b2)
  (doc 'type (-> Block Block Boolean))
  (doc 'description "Check if two blocks are structurally equal")
  (and (symbol=? (block-tag b1) (block-tag b2))
       (bytevector=? (block-payload b1) (block-payload b2))
       (refs-equal? (block-refs b1) (block-refs b2))))

(define (refs-equal? refs1 refs2)
  (doc 'type (-> (Vector Hash) (Vector Hash) Boolean))
  (doc 'description "Check if two ref vectors contain the same hashes in order")
  (and (= (vector-length refs1) (vector-length refs2))
       (let loop ([i 0])
            (or (= i (vector-length refs1))
                (and (bytevector=? (vector-ref refs1 i) (vector-ref refs2 i))
                     (loop (+ i 1)))))))

(doc 'section 'diff-computation)
(doc 'note "DiffResult = (same | different | missing-left | missing-right)")
(doc 'note "Diff = ((field . Symbol) (status . DiffResult) (left . Value) (right . Value))")

(define (compute-block-diff b1 b2)
  (doc 'type (-> Block Block (List Diff)))
  (doc 'description "Compute differences between two blocks")
  (let ([tag-diff (if (symbol=? (block-tag b1) (block-tag b2))
                      `((field . tag) (status . same) (left . ,(block-tag b1)) (right . ,(block-tag b2)))
                      `((field . tag) (status . different) (left . ,(block-tag b1)) (right . ,(block-tag b2))))]
        [payload-diff (if (bytevector=? (block-payload b1) (block-payload b2))
                          `((field . payload) (status . same)
                            (left . ,(block-payload b1)) (right . ,(block-payload b2)))
                          `((field . payload) (status . different)
                            (left . ,(block-payload b1)) (right . ,(block-payload b2))))]
        [refs-diff (diff-refs (block-refs b1) (block-refs b2))])
       (list tag-diff payload-diff refs-diff)))

(define (diff-refs refs1 refs2)
  (doc 'type (-> (Vector Hash) (Vector Hash) Diff))
  (doc 'description "Compute differences between two ref vectors")
  (let ([len1 (vector-length refs1)]
        [len2 (vector-length refs2)])
       (cond
        [(and (= len1 0) (= len2 0))
         `((field . refs) (status . same) (left . #()) (right . #()))]
        [(and (= len1 len2)
              (let loop ([i 0])
                   (or (= i len1)
                       (and (bytevector=? (vector-ref refs1 i) (vector-ref refs2 i))
                            (loop (+ i 1))))))
         `((field . refs) (status . same) (left . ,refs1) (right . ,refs2))]
        [else
         `((field . refs) (status . different)
           (left . ,refs1) (right . ,refs2)
           (details . ,(diff-refs-detailed refs1 refs2)))])))

(define (diff-refs-detailed refs1 refs2)
  (doc 'type (-> (Vector Hash) (Vector Hash) (List RefDiff)))
  (doc 'description "Detailed comparison of refs showing which are same/different/missing")
  (let ([len1 (vector-length refs1)]
        [len2 (vector-length refs2)]
        [max-len (max (vector-length refs1) (vector-length refs2))])
       (let loop ([i 0] [diffs '()])
            (if (= i max-len)
                (reverse diffs)
                (let ([has-left (< i len1)]
                      [has-right (< i len2)])
                     (cond
                      [(and has-left has-right)
                       (let ([left-hash (vector-ref refs1 i)]
                             [right-hash (vector-ref refs2 i)])
                            (if (bytevector=? left-hash right-hash)
                                (loop (+ i 1) (cons `((index . ,i) (status . same) (hash . ,left-hash)) diffs))
                                (loop (+ i 1) (cons `((index . ,i) (status . different)
                                                      (left . ,left-hash) (right . ,right-hash)) diffs))))]
                      [has-left
                       (loop (+ i 1) (cons `((index . ,i) (status . missing-right)
                                             (hash . ,(vector-ref refs1 i))) diffs))]
                      [has-right
                       (loop (+ i 1) (cons `((index . ,i) (status . missing-left)
                                             (hash . ,(vector-ref refs2 i))) diffs))]
                      [else (loop (+ i 1) diffs)]))))))

(doc 'section 'similarity)

(define (compute-block-similarity b1 b2)
  (doc 'type (-> Block Block Real))
  (doc 'description "Compute similarity score between 0.0 and 1.0")
  (doc 'note "1.0 means identical, 0.0 means completely different")
  (let ([tag-score (if (symbol=? (block-tag b1) (block-tag b2)) 1.0 0.0)]
        [payload-score (payload-similarity (block-payload b1) (block-payload b2))]
        [refs-score (refs-similarity (block-refs b1) (block-refs b2))])
       ;; Weighted average: tag=20%, payload=40%, refs=40%
       (+ (* 0.2 tag-score)
          (* 0.4 payload-score)
          (* 0.4 refs-score))))

(define (payload-similarity p1 p2)
  (doc 'type (-> Bytevector Bytevector Real))
  (doc 'description "Compute similarity of payloads using byte overlap")
  (let ([len1 (bytevector-length p1)]
        [len2 (bytevector-length p2)])
       (cond
        [(and (= len1 0) (= len2 0)) 1.0]
        [(or (= len1 0) (= len2 0)) 0.0]
        [else
         (let ([min-len (min len1 len2)]
               [max-len (max len1 len2)]
               [same-bytes (let loop ([i 0] [count 0])
                                (if (= i min-len)
                                    count
                                    (loop (+ i 1)
                                          (if (= (bytevector-u8-ref p1 i)
                                                 (bytevector-u8-ref p2 i))
                                              (+ count 1)
                                              count))))])
              (/ (exact->inexact same-bytes) (exact->inexact max-len)))])))

(define (refs-similarity refs1 refs2)
  (doc 'type (-> (Vector Hash) (Vector Hash) Real))
  (doc 'description "Compute similarity of ref vectors")
  (let ([len1 (vector-length refs1)]
        [len2 (vector-length refs2)])
       (cond
        [(and (= len1 0) (= len2 0)) 1.0]
        [(or (= len1 0) (= len2 0)) 0.0]
        [else
         (let ([min-len (min len1 len2)]
               [max-len (max len1 len2)]
               [same-refs (let loop ([i 0] [count 0])
                               (if (= i min-len)
                                   count
                                   (loop (+ i 1)
                                         (if (bytevector=? (vector-ref refs1 i)
                                                           (vector-ref refs2 i))
                                             (+ count 1)
                                             count))))])
              (/ (exact->inexact same-refs) (exact->inexact max-len)))])))

(doc 'section 'display)

(define (display-block-summary block)
  (doc 'type (-> Block Void))
  (doc 'description "Display a concise summary of a block")
  (display (format "  Tag: ~a\n" (block-tag block)))
  (display (format "  Payload: ~a bytes\n" (bytevector-length (block-payload block))))
  (display (format "  Refs: ~a\n" (vector-length (block-refs block)))))

(define (display-diff diff)
  (doc 'type (-> (List Diff) Void))
  (doc 'description "Display the diff results")
  (for-each
   (lambda (field-diff)
           (let ([field (cdr (assq 'field field-diff))]
                 [status (cdr (assq 'status field-diff))])
                (case status
                      [(same)
                       (display (format "  ✓ ~a: identical\n" field))]
                      [(different)
                       (display (format "  ✗ ~a: DIFFERENT\n" field))
                       (when (eq? field 'refs)
                             (let ([details (assq 'details field-diff)])
                                  (when details
                                        (display "    Ref differences:\n")
                                        (for-each
                                         (lambda (ref-diff)
                                                 (let ([idx (cdr (assq 'index ref-diff))]
                                                       [ref-status (cdr (assq 'status ref-diff))])
                                                      (case ref-status
                                                            [(same)
                                                             (display (format "      [~a] same\n" idx))]
                                                            [(different)
                                                             (display (format "      [~a] DIFFERENT\n" idx))]
                                                            [(missing-left)
                                                             (display (format "      [~a] only in right\n" idx))]
                                                            [(missing-right)
                                                             (display (format "      [~a] only in left\n" idx))])))
                                         (cdr details)))))]
                      [(missing-left)
                       (display (format "  ⚠ ~a: missing in left block\n" field))]
                      [(missing-right)
                       (display (format "  ⚠ ~a: missing in right block\n" field))])))
   diff))

(doc 'section 'public-api)

(define (block-diff fs hash1 hash2)
  (doc 'type (-> FS Hash Hash Void))
  (doc 'description "Compare two blocks and display differences")
  (doc 'export #t)
  (let ([store (fs-make-cas fs)]
        [h1 (if (string? hash1) (hex->hash hash1) hash1)]
        [h2 (if (string? hash2) (hex->hash hash2) hash2)])

       (display "\n╔══════════════════════════════════════════════════════════════╗\n")
       (display "║                      BLOCK DIFF                              ║\n")
       (display "╚══════════════════════════════════════════════════════════════╝\n\n")

       (let ([b1-result (fetch store h1)]
             [b2-result (fetch store h2)])
            (cond
             [(error? b1-result)
              (display (format "Error: Left block not found: ~a\n" (error-message b1-result)))]
             [(error? b2-result)
              (display (format "Error: Right block not found: ~a\n" (error-message b2-result)))]
             [else
              (let ([b1 (result-value b1-result)]
                    [b2 (result-value b2-result)])
                   (display (format "Left:  ~a\n" (hash->hex h1)))
                   (display-block-summary b1)
                   (display "\n")
                   (display (format "Right: ~a\n" (hash->hex h2)))
                   (display-block-summary b2)
                   (display "\n")

                   (if (block-equal? b1 b2)
                       (display "✓ Blocks are IDENTICAL\n")
                       (begin
                        (display "Differences:\n")
                        (let ([diff (compute-block-diff b1 b2)])
                             (display-diff diff))
                        (display "\n")
                        (let ([similarity (compute-block-similarity b1 b2)])
                             (display (format "Similarity: ~a% (~a)\n"
                                              (exact (round (* similarity 100)))
                                              (cond
                                               [(>= similarity 0.9) "very similar"]
                                               [(>= similarity 0.7) "similar"]
                                               [(>= similarity 0.5) "somewhat similar"]
                                               [else "very different"])))))))]))))

(define (block-diff-normalized fs hash1 hash2)
  (doc 'type (-> FS Hash Hash Void))
  (doc 'description "Compare normalized forms of two blocks (ignore variable names)")
  (doc 'export #t)
  (let ([store (fs-make-cas fs)]
        [h1 (if (string? hash1) (hex->hash hash1) hash1)]
        [h2 (if (string? hash2) (hex->hash hash2) hash2)])

       (display "\n╔══════════════════════════════════════════════════════════════╗\n")
       (display "║              NORMALIZED BLOCK DIFF                           ║\n")
       (display "╚══════════════════════════════════════════════════════════════╝\n\n")

       (let ([b1-result (fetch store h1)]
             [b2-result (fetch store h2)])
            (cond
             [(error? b1-result)
              (display (format "Error: Left block not found\n"))]
             [(error? b2-result)
              (display (format "Error: Right block not found\n"))]
             [else
              (let* ([b1 (result-value b1-result)]
                     [b2 (result-value b2-result)]
                     ;; Try to interpret payloads as S-expressions
                     [sexpr1 (guard (e [else #f])
                                    (read (open-input-string
                                           (utf8->string (block-payload b1)))))]
                     [sexpr2 (guard (e [else #f])
                                    (read (open-input-string
                                           (utf8->string (block-payload b2)))))])
                    (cond
                     [(not sexpr1)
                      (display "Left block payload is not a valid S-expression\n")]
                     [(not sexpr2)
                      (display "Right block payload is not a valid S-expression\n")]
                     [else
                      (let ([norm1 (normalize sexpr1)]
                            [norm2 (normalize sexpr2)])
                           (display (format "Left (normalized):  ~s\n" norm1))
                           (display (format "Right (normalized): ~s\n\n" norm2))
                           (if (equal? norm1 norm2)
                               (display "✓ Normalized forms are IDENTICAL (same structure, different variable names)\n")
                               (display "✗ Normalized forms are DIFFERENT (different structure)\n")))]))]))))

(define (block-similarity fs hash1 hash2)
  (doc 'type (-> FS Hash Hash Void))
  (doc 'description "Compute and display similarity score between two blocks")
  (doc 'export #t)
  (let ([store (fs-make-cas fs)]
        [h1 (if (string? hash1) (hex->hash hash1) hash1)]
        [h2 (if (string? hash2) (hex->hash hash2) hash2)])

       (let ([b1-result (fetch store h1)]
             [b2-result (fetch store h2)])
            (cond
             [(error? b1-result)
              (display "Error: Left block not found\n")]
             [(error? b2-result)
              (display "Error: Right block not found\n")]
             [else
              (let* ([b1 (result-value b1-result)]
                     [b2 (result-value b2-result)]
                     [sim (compute-block-similarity b1 b2)])
                    (display (format "\nSimilarity: ~a%\n" (exact (round (* sim 100)))))
                    (display (format "Status: ~a\n"
                                     (cond
                                      [(>= sim 0.95) "nearly identical"]
                                      [(>= sim 0.8) "very similar"]
                                      [(>= sim 0.6) "similar"]
                                      [(>= sim 0.4) "somewhat similar"]
                                      [else "very different"]))))]))))

(define (block-compare-many fs hash-list)
  (doc 'type (-> FS (List Hash) Void))
  (doc 'description "Compare multiple blocks and show pairwise similarities")
  (doc 'export #t)
  (let ([store (fs-make-cas fs)]
        [hashes (map (lambda (h) (if (string? h) (hex->hash h) h)) hash-list)])

       (display "\n╔══════════════════════════════════════════════════════════════╗\n")
       (display "║              MULTI-BLOCK COMPARISON                          ║\n")
       (display "╚══════════════════════════════════════════════════════════════╝\n\n")

       ;; Fetch all blocks
       (let ([blocks (map (lambda (h)
                                  (let ([result (fetch store h)])
                                       (if (error? result)
                                           #f
                                           (result-value result))))
                          hashes)])

            ;; Check for errors
            (let ([errors (filter (lambda (b) (not b)) blocks)])
                 (unless (null? errors)
                         (display (format "Warning: ~a blocks could not be fetched\n\n" (length errors)))))

            ;; Compute pairwise similarities
            (display "Pairwise similarities:\n\n")
            (let loop-i ([i 0])
                 (when (< i (length blocks))
                       (let loop-j ([j (+ i 1)])
                            (when (< j (length blocks))
                                  (let ([bi (list-ref blocks i)]
                                        [bj (list-ref blocks j)])
                                       (when (and bi bj)
                                             (let ([sim (compute-block-similarity bi bj)])
                                                  (display (format "  [~a] ↔ [~a]: ~a%\n"
                                                                   i j (exact (round (* sim 100))))))))
                                  (loop-j (+ j 1))))
                       (loop-i (+ i 1)))))))

(doc 'section 'helpers)

(define (hex->hash hex-str)
  (doc 'type (-> String Hash))
  (doc 'description "Convert hex string to hash bytevector")
  (let* ([len (string-length hex-str)]
         [hash (make-bytevector (/ len 2))])
        (let loop ([i 0])
             (when (< i (/ len 2))
                   (let ([high (hex-digit->int (string-ref hex-str (* i 2)))]
                         [low (hex-digit->int (string-ref hex-str (+ (* i 2) 1)))])
                        (bytevector-u8-set! hash i (+ (* high 16) low))
                        (loop (+ i 1)))))
        hash))

(define (hex-digit->int c)
  (doc 'type (-> Char Nat))
  (doc 'description "Convert hex digit character to integer")
  (cond
   [(char<=? #\0 c #\9) (- (char->integer c) (char->integer #\0))]
   [(char<=? #\a c #\f) (+ 10 (- (char->integer c) (char->integer #\a)))]
   [(char<=? #\A c #\F) (+ 10 (- (char->integer c) (char->integer #\A)))]
   [else 0]))

(define (hash->hex h)
  (doc 'type (-> Hash String))
  (doc 'description "Convert hash to hex string")
  (let* ([len (bytevector-length h)]
         [hex-chars (make-string (* len 2))])
        (let loop ([i 0])
             (when (< i len)
                   (let ([byte (bytevector-u8-ref h i)])
                        (string-set! hex-chars (* i 2) (int->hex-digit (quotient byte 16)))
                        (string-set! hex-chars (+ (* i 2) 1) (int->hex-digit (remainder byte 16)))
                        (loop (+ i 1)))))
        hex-chars))

(define (int->hex-digit n)
  (doc 'type (-> Nat Char))
  (doc 'description "Convert integer (0-15) to hex digit character")
  (if (< n 10)
      (integer->char (+ (char->integer #\0) n))
      (integer->char (+ (char->integer #\a) (- n 10)))))

(display "Block diff utility loaded.\n")
(display "Usage:\n")
(display "  (block-diff fs hash1 hash2)\n")
(display "  (block-diff-normalized fs hash1 hash2)\n")
(display "  (block-similarity fs hash1 hash2)\n")
(display "  (block-compare-many fs (list hash1 hash2 hash3))\n")
