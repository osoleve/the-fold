;;; store-doctor.ss — Diagnose and repair corrupted store data
;;;
;;; Scans .store for corrupted blocks, malformed forum posts,
;;; and data integrity issues.
;;;
;;; Usage:
;;;   (load "shell/tools/store-doctor.ss")
;;;   (store-doctor-scan)           ; Report issues
;;;   (store-doctor-purge! issues)  ; Purge reported issues

(load "core/base/prelude.ss")
(load "core/blocks/block.ss")
(load "core/base/sha256.ss")
(load "shell/fs.ss")

;;; ============================================================
;;; Validation Helpers
;;; ============================================================

;;; contains-null-byte? : String → Boolean
;;; Check if string contains NUL bytes (common corruption indicator)
(define (contains-null-byte? str)
  (let loop ([i 0])
       (cond
        [(>= i (string-length str)) #f]
        [(char=? (string-ref str i) #\nul) #t]
        [else (loop (+ i 1))])))

;;; valid-timestamp? : Any → Boolean
;;; Check if value is a valid ISO8601 timestamp string
(define (valid-timestamp? ts)
  (and (string? ts)
       (>= (string-length ts) 19)
       (not (contains-null-byte? ts))
       ;; Check basic format: YYYY-MM-DDTHH:MM:SS
       (char-numeric? (string-ref ts 0))
       (char-numeric? (string-ref ts 1))
       (char-numeric? (string-ref ts 2))
       (char-numeric? (string-ref ts 3))
       (char=? (string-ref ts 4) #\-)
       (char-numeric? (string-ref ts 5))
       (char-numeric? (string-ref ts 6))
       (char=? (string-ref ts 7) #\-)
       (char-numeric? (string-ref ts 8))
       (char-numeric? (string-ref ts 9))
       (char=? (string-ref ts 10) #\T)
       (char-numeric? (string-ref ts 11))
       (char-numeric? (string-ref ts 12))
       (char=? (string-ref ts 13) #\:)
       (char-numeric? (string-ref ts 14))
       (char-numeric? (string-ref ts 15))
       (char=? (string-ref ts 16) #\:)
       (char-numeric? (string-ref ts 17))
       (char-numeric? (string-ref ts 18))))

;;; valid-channel? : Any → Boolean
(define (valid-channel? ch)
  (and (symbol? ch)
       (memq ch '(philosophy engineering poetry art design
                  requests wishlist chat announcements general))))

;;; valid-tier? : Any → Boolean
(define (valid-tier? tier)
  (and (symbol? tier)
       (memq tier '(shepherd builder player outsider))))

;;; ============================================================
;;; Block Validation
;;; ============================================================

;;; validate-block : Block Hash → (List Issue)
;;; Validate a single block, return list of issues found
(define (validate-block blk hash)
  (let ([issues '()])
       ;; Check block structure
       (unless (and (pair? blk) (eq? (car blk) 'block))
               (set! issues (cons `(structural "Not a valid block structure") issues)))
       
       (when (and (pair? blk) (eq? (car blk) 'block))
             (let ([tag (block-tag blk)]
                   [payload (block-payload blk)]
                   [refs (block-refs blk)])
                  
                  ;; Check tag
                  (unless (symbol? tag)
                          (set! issues (cons `(tag "Tag is not a symbol: ~a" ,tag) issues)))
                  
                  ;; Check payload
                  (unless (bytevector? payload)
                          (set! issues (cons `(payload "Payload is not a bytevector") issues)))
                  
                  ;; Check for NUL bytes in text payloads
                  (when (and (bytevector? payload)
                             (memq tag '(post entity relation note)))
                        (let ([str (utf8->string payload)])
                             (when (contains-null-byte? str)
                                   (set! issues (cons `(null-byte "Payload contains NUL bytes") issues)))))
                  
                  ;; Check refs
                  (unless (vector? refs)
                          (set! issues (cons `(refs "Refs is not a vector") issues)))
                  
                  (when (vector? refs)
                        (let loop ([i 0])
                             (when (< i (vector-length refs))
                                   (let ([ref (vector-ref refs i)])
                                        (unless (and (bytevector? ref) (= (bytevector-length ref) 32))
                                                (set! issues (cons `(ref "Invalid ref at index ~a" ,i) issues))))
                                   (loop (+ i 1)))))
                  
                  ;; Verify hash integrity
                  (let ([computed-hash (hash-block blk)])
                       (unless (bytevector=? computed-hash hash)
                               (set! issues (cons `(hash-mismatch "Stored hash doesn't match computed hash") issues))))))
       
       issues))

;;; validate-post-metadata : Block → (List Issue)
;;; Validate forum post metadata
(define (validate-post-metadata blk)
  (let ([issues '()])
       (when (eq? (block-tag blk) 'post)
             (guard (ex [else (set! issues (cons `(parse "Failed to parse metadata: ~a" ,(condition-message ex)) issues))])
                    (let* ([payload-str (utf8->string (block-payload blk))]
                           [meta (read (open-input-string payload-str))])
                          
                          ;; Check it's an alist
                          (unless (and (list? meta) (every pair? meta))
                                  (set! issues (cons `(meta-format "Metadata is not an alist") issues)))
                          
                          (when (and (list? meta) (every pair? meta))
                                ;; Check timestamp
                                (let ([ts (assq 'timestamp meta)])
                                     (when ts
                                           (unless (valid-timestamp? (cdr ts))
                                                   (set! issues (cons `(timestamp "Invalid timestamp: ~a" ,(cdr ts)) issues)))))
                                
                                ;; Check channel
                                (let ([ch (assq 'channel meta)])
                                     (when ch
                                           (unless (valid-channel? (cdr ch))
                                                   (set! issues (cons `(channel "Invalid channel: ~a" ,(cdr ch)) issues)))))
                                
                                ;; Check tier
                                (let ([tier (assq 'tier meta)])
                                     (when tier
                                           (unless (valid-tier? (cdr tier))
                                                   (set! issues (cons `(tier "Invalid tier: ~a" ,(cdr tier)) issues)))))
                                
                                ;; Check author
                                (let ([author (assq 'author meta)])
                                     (when author
                                           (unless (symbol? (cdr author))
                                                   (set! issues (cons `(author "Author is not a symbol: ~a" ,(cdr author)) issues)))))
                                
                                ;; Check body for NUL bytes
                                (let ([body (assq 'body meta)])
                                     (when body
                                           (when (and (string? (cdr body)) (contains-null-byte? (cdr body)))
                                                 (set! issues (cons `(body-null "Body contains NUL bytes") issues)))))))))
       issues))

;;; ============================================================
;;; Store Scanning
;;; ============================================================

;;; store-doctor-scan : → (List (Hash . Issues))
;;; Scan entire store and return list of problematic blocks
(define (store-doctor-scan)
  (printf "=== Store Doctor Scan ===~%~%")
  (let* ([fs (mint-fs-capability ".store")]
         [all-hashes (fs-all-hashes fs)]
         [total (length all-hashes)]
         [problems '()]
         [scanned 0])
        
        (printf "Scanning ~a blocks...~%~%" total)
        
        (for-each
         (lambda (hash)
                 (set! scanned (+ scanned 1))
                 (when (= (modulo scanned 100) 0)
                       (printf "  Progress: ~a/~a~%" scanned total))
                 
                 (let ([blk (fs-fetch fs hash)])
                      (if (not blk)
                          (set! problems (cons (cons hash '((missing "Block not found in store"))) problems))
                          (let* ([block-issues (validate-block blk hash)]
                                 [post-issues (validate-post-metadata blk)]
                                 [all-issues (append block-issues post-issues)])
                                (unless (null? all-issues)
                                        (set! problems (cons (cons hash all-issues) problems)))))))
         all-hashes)
        
        (printf "~%=== Scan Complete ===~%")
        (printf "Total blocks: ~a~%" total)
        (printf "Problems found: ~a~%~%" (length problems))
        
        (when (not (null? problems))
              (printf "Problem details:~%")
              (for-each
               (lambda (problem)
                       (let ([hash (car problem)]
                             [issues (cdr problem)])
                            (printf "~%Block: ~a~%" (bytevector->hex-string hash))
                            (for-each
                             (lambda (issue)
                                     (printf "  - [~a] ~a~%" (car issue) (cadr issue)))
                             issues)))
               problems))
        
        problems))

;;; ============================================================
;;; Quick Checks
;;; ============================================================

;;; store-doctor-check-channels : → (List Issue)
;;; Quick check of channel head blocks
(define (store-doctor-check-channels)
  (printf "=== Checking Channel Heads ===~%~%")
  (let* ([fs (mint-fs-capability ".store")]
         [channels '(philosophy engineering poetry art design
                     requests wishlist chat announcements general)]
         [issues '()])
        
        (for-each
         (lambda (channel)
                 (let ([head (fs-read-head fs channel)])
                      (if (not head)
                          (printf "  ~a: (no posts)~%" channel)
                          (let ([blk (fs-fetch fs head)])
                               (if (not blk)
                                   (begin
                                    (printf "  ~a: DANGLING HEAD - block not found!~%" channel)
                                    (set! issues (cons `(,channel dangling-head ,head) issues)))
                                   (let ([post-issues (validate-post-metadata blk)])
                                        (if (null? post-issues)
                                            (printf "  ~a: OK~%" channel)
                                            (begin
                                             (printf "  ~a: ISSUES - ~a~%" channel post-issues)
                                             (set! issues (cons `(,channel ,@post-issues) issues))))))))))
         channels)
        
        (printf "~%")
        issues))

;;; ============================================================
;;; Repair Functions
;;; ============================================================

;;; store-doctor-purge! : (List (Hash . Issues)) → Void
;;; Purge problematic blocks (USE WITH CAUTION)
(define (store-doctor-purge! problems)
  (printf "=== Purging ~a problematic blocks ===~%~%" (length problems))
  (printf "WARNING: This will permanently delete blocks!~%")
  (printf "Press Ctrl+C to cancel within 5 seconds...~%~%")
  
  ;; Give time to cancel
  (sleep (make-time 'time-duration 0 5))
  
  (let ([fs (mint-fs-capability ".store")]
        [purged 0])
       
       (for-each
        (lambda (problem)
                (let ([hash (car problem)])
                     (printf "Purging: ~a~%" (bytevector->hex-string hash))
                     ;; Unpin first if pinned
                     (when (fs-pinned? fs hash)
                           (fs-unpin! fs hash))
                     ;; Note: We can't directly delete from hashtable without modifying fs.ss
                     ;; For now, just unpin and log - actual deletion requires GC
                     (set! purged (+ purged 1))))
        problems)
       
       (printf "~%Unpinned ~a blocks. Run (gc-store!) to reclaim space.~%" purged)))

;;; store-doctor-fix-head! : Symbol Hash → Void
;;; Fix a dangling channel head by setting to valid block or #f
(define (store-doctor-fix-head! channel new-head)
  (let ([fs (mint-fs-capability ".store")])
       (if new-head
           (let ([blk (fs-fetch fs new-head)])
                (if blk
                    (begin
                     (fs-write-head! fs channel new-head)
                     (printf "Set ~a head to ~a~%" channel (bytevector->hex-string new-head)))
                    (error 'store-doctor-fix-head! "New head block not found")))
           (begin
            ;; Clear the head by writing empty bytevector as marker
            ;; The fs layer uses #f for "no head", we need to check how to clear
            (printf "Note: To clear a channel head, manually remove from .store/heads/~a~%" channel)))))

;;; ============================================================
;;; Utility
;;; ============================================================

;;; bytevector->hex-string : Bytevector → String
(define (bytevector->hex-string bv)
  (let* ([len (bytevector-length bv)]
         [hex-chars "0123456789abcdef"]
         [result (make-string (* len 2))])
        (do ([i 0 (+ i 1)])
            [(= i len) result]
            (let ([b (bytevector-u8-ref bv i)])
                 (string-set! result (* i 2) (string-ref hex-chars (quotient b 16)))
                 (string-set! result (+ (* i 2) 1) (string-ref hex-chars (modulo b 16)))))))

;;; ============================================================
;;; Entry Point
;;; ============================================================

(printf "Store Doctor loaded.~%")
(printf "Commands:~%")
(printf "  (store-doctor-scan)           - Full store scan~%")
(printf "  (store-doctor-check-channels) - Quick channel check~%")
(printf "  (store-doctor-purge! issues)  - Purge bad blocks~%")
(printf "  (store-doctor-fix-head! ch h) - Fix channel head~%")
