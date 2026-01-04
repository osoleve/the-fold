#!/usr/bin/env scheme --script
;;; migrate-blocks.ss — Migrate all blocks to new 33-byte ref format
;;;
;;; Strategy:
;;; 1. Read all blocks (using backward-compat reader)
;;; 2. Build old-hash -> new-hash mapping
;;; 3. Rewrite all blocks with updated refs
;;; 4. Delete old block files

(load "core/base/prelude.ss")
(load "core/blocks/block.ss")
(load "core/base/sha256.ss")

(define store-path ".store")

;;; Get all block files
(define (get-all-block-files)
  (let ([objects-dir (string-append store-path "/objects")])
       (apply append
              (map (lambda (prefix)
                           (let ([prefix-dir (string-append objects-dir "/" prefix)])
                                (if (file-directory? prefix-dir)
                                    (map (lambda (f) (string-append prefix-dir "/" f))
                                         (directory-list prefix-dir))
                                    '())))
                   (directory-list objects-dir)))))

;;; Read block from file
(define (read-block-file path)
  (guard (e [else #f])
         (let* ([port (open-file-input-port path)]
                [bytes (get-bytevector-all port)])
               (close-port port)
               (bytes->block bytes))))

;;; Compute hash of block
(define (compute-block-hash blk)
  (sha256 (block->bytes blk)))

;;; Hash to hex string
(define (hash->hex bv)
  (apply string-append
         (map (lambda (i)
                      (let* ([b (bytevector-u8-ref bv i)]
                             [hi (bitwise-arithmetic-shift-right b 4)]
                             [lo (bitwise-and b #xf)])
                            (string (integer->char (+ (if (< hi 10) 48 87) hi))
                                    (integer->char (+ (if (< lo 10) 48 87) lo)))))
              (iota (bytevector-length bv)))))

;;; Get path from hash
(define (hash->path hash)
  (let ([hex (hash->hex hash)])
       (string-append store-path "/objects/"
                      (substring hex 0 2) "/" hex)))

;;; Extract hash from file path
(define (path->hash path)
  (let* ([parts (string-split path #\/)]
         [filename (car (reverse parts))])
        (hex->bytevector filename)))

;;; Hex string to bytevector
(define (hex->bytevector hex)
  (let* ([len (div (string-length hex) 2)]
         [bv (make-bytevector len)])
        (do ([i 0 (+ i 1)])
            ((= i len) bv)
            (let ([hi (string-ref hex (* i 2))]
                  [lo (string-ref hex (+ (* i 2) 1))])
                 (bytevector-u8-set! bv i
                                     (+ (* 16 (hex-digit hi)) (hex-digit lo)))))))

(define (hex-digit c)
  (cond [(char<=? #\0 c #\9) (- (char->integer c) 48)]
        [(char<=? #\a c #\f) (+ 10 (- (char->integer c) 97))]
        [(char<=? #\A c #\F) (+ 10 (- (char->integer c) 65))]
        [else 0]))

;;; Check if block needs migration (has old-format refs in file)
(define (needs-migration? path)
  (guard (e [else #f])
         (let* ([port (open-file-input-port path)]
                [bytes (get-bytevector-all port)])
               (close-port port)
               (let* ([bv-len (bytevector-length bytes)]
                      [tag-len (bytes-le->u32 bytes 0)]
                      [pos (+ 4 tag-len)]
                      [payload-len (bytes-le->u32 bytes pos)]
                      [pos (+ pos 4 payload-len)]
                      [refs-count (bytes-le->u32 bytes pos)]
                      [pos (+ pos 4)]
                      [remaining (- bv-len pos)])
                     (and (> refs-count 0)
                          (= remaining (* refs-count 32))
                          (not (= remaining (* refs-count 33))))))))

;;; Ensure parent directory exists
(define (ensure-parent-dir! path)
  (let* ([parts (string-split path #\/)]
         [dir-parts (reverse (cdr (reverse parts)))]
         [dir (string-join dir-parts "/")])
        (unless (file-exists? dir)
                (mkdir dir))))

;;; Simple alist-based hash lookup
(define (alist-ref alist key default)
  (let loop ([lst alist])
       (cond [(null? lst) default]
             [(bytevector=? (caar lst) key) (cdar lst)]
             [else (loop (cdr lst))])))

;;; Main migration
(define (migrate!)
  (printf "=== Block Migration ===~%~%")
  
  (let ([files (get-all-block-files)]
        [hash-map '()]
        [blocks '()]
        [migrated 0]
        [unchanged 0]
        [errors 0])
       
       ;; Phase 1: Read all blocks and build hash mapping
       (printf "Phase 1: Reading blocks and computing new hashes...~%")
       (for-each
        (lambda (path)
                (let ([blk (read-block-file path)])
                     (if blk
                         (let* ([old-hash (path->hash path)]
                                [new-hash (compute-block-hash blk)])
                               (set! hash-map (cons (cons old-hash new-hash) hash-map))
                               (set! blocks (cons (list path old-hash blk) blocks)))
                         (begin
                          (printf "  ERROR reading: ~a~%" path)
                          (set! errors (+ errors 1))))))
        files)
       (printf "  Read ~a blocks~%" (length blocks))
       
       ;; Phase 2: Rewrite blocks with updated refs
       (printf "~%Phase 2: Rewriting blocks with new format...~%")
       (for-each
        (lambda (entry)
                (let* ([path (car entry)]
                       [old-hash (cadr entry)]
                       [blk (caddr entry)]
                       [refs (block-refs blk)]
                       ;; Update refs to new hashes (extract hash portion from address)
                       [new-refs (vector-map
                                  (lambda (ref)
                                          ;; ref is 33 bytes: version + hash
                                          ;; Look up if this hash has a new mapping
                                          (let* ([hash-portion (make-bytevector 32)])
                                                (bytevector-copy! ref 1 hash-portion 0 32)
                                                (let ([new-hash (alist-ref hash-map hash-portion #f)])
                                                     (if new-hash
                                                         ;; Create new address with version + new hash
                                                         (let ([new-ref (make-bytevector 33)])
                                                              (bytevector-u8-set! new-ref 0 (bytevector-u8-ref ref 0))
                                                              (bytevector-copy! new-hash 0 new-ref 1 32)
                                                              new-ref)
                                                         ;; Keep original ref
                                                         ref))))
                                  refs)]
                       [new-blk (make-block (block-tag blk) (block-payload blk) new-refs)]
                       [new-bytes (block->bytes new-blk)]
                       [new-hash (sha256 new-bytes)]
                       [new-path (hash->path new-hash)])
                      ;; Write new block
                      (ensure-parent-dir! new-path)
                      (let ([out (open-file-output-port new-path (file-options no-fail))])
                           (put-bytevector out new-bytes)
                           (close-port out))
                      ;; Delete old if different
                      (if (bytevector=? old-hash new-hash)
                          (set! unchanged (+ unchanged 1))
                          (begin
                           (delete-file path)
                           (set! migrated (+ migrated 1))
                           (when (< migrated 5)
                                 (printf "  ~a -> ~a~%"
                                         (substring (hash->hex old-hash) 0 8)
                                         (substring (hash->hex new-hash) 0 8)))))))
        blocks)
       
       (printf "~%=== Migration Complete ===~%")
       (printf "  Migrated:  ~a blocks~%" migrated)
       (printf "  Unchanged: ~a blocks~%" unchanged)
       (printf "  Errors:    ~a blocks~%" errors)))

;; Run
(migrate!)
