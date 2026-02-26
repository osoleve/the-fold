;;; @module rlm2-memory
;;; @description CAS-backed persistent memory for RLM agents.
;;; Loaded by rlm2-drive.ss — requires CAS storage and block primitives.

;;; --- System Memory (CAS-addressed, persistent across runs) ---
;;;
;;; Memory is stored as content-addressed blocks in .store/.
;;; Each snapshot is a block:
;;;   tag:     rlm2/memory
;;;   payload: sexp-encoded list of (key text timestamp) entries
;;;   refs:    (vector prev-hash) for history, or #() for first snapshot
;;;
;;; A head pointer at .store/heads/rlm2-memory/{agent-id}.head
;;; tracks the current snapshot hash. Migration from the legacy
;;; .rlm/memories.sexp flat file happens automatically on first load.

(define *rlm2-system-memory-cache* #f)
(define *rlm2-system-memory-index* #f)
(define *rlm2-system-memory-head*  #f)  ; current head hash (bytevector or #f)

(define *rlm2-memory-heads-dir* ".store/heads/rlm2-memory")
(define *rlm2-memory-agent-id* "default")

(define (rlm2-memory-head-path)
  (string-append *rlm2-memory-heads-dir* "/" *rlm2-memory-agent-id* ".head"))

(define (rlm2-ensure-memory-heads-dir!)
  (unless (file-exists? ".store")
    (mkdir ".store"))
  (unless (file-exists? ".store/heads")
    (mkdir ".store/heads"))
  (unless (file-exists? *rlm2-memory-heads-dir*)
    (mkdir *rlm2-memory-heads-dir*)))

(define (rlm2-read-memory-head)
  (let ([path (rlm2-memory-head-path)])
    (guard (e [else #f])
      (if (file-exists? path)
          (let* ([content (call-with-input-file path
                            (lambda (port) (get-line port)))]
                 [trimmed (string-trim content)])
            (if (>= (string-length trimmed) 64)
                (hex->hash trimmed)
                #f))
          #f))))

(define (rlm2-write-memory-head! hash)
  (rlm2-ensure-memory-heads-dir!)
  (let ([path (rlm2-memory-head-path)])
    (with-file-lock path
      (lambda ()
        (call-with-atomic-output-file path
          (lambda (port)
            (put-string port (hash->hex hash))
            (newline port))
          '(replace))))))

(define (rlm2-memory-block->entries blk)
  "Decode a memory block's payload back to a list of entries."
  (guard (ex [else '()])
    (let ([str (utf8->string (block-payload blk))])
      (let ([data (read (open-input-string str))])
        (if (list? data) data '())))))

(define (rlm2-store-memory-block! entries prev-hash)
  "Store a memory snapshot block. Returns the new hash."
  (let* ([payload (string->utf8 (format "~s" entries))]
         [refs (if prev-hash (vector prev-hash) (vector))]
         [blk (make-block 'rlm2/memory payload refs)]
         [hash (store-persistent! blk)])
    hash))

;;; Legacy migration: import .rlm/memories.sexp into CAS on first load
(define (rlm2-migrate-legacy-memory!)
  (let ([legacy-path (string-append (current-directory) "/.rlm/memories.sexp")])
    (if (file-exists? legacy-path)
        (guard (ex [else
                    (format (current-error-port)
                      "[RLM] WARNING: failed to migrate legacy memory: ~a~%"
                      (if (message-condition? ex) (condition-message ex) ex))
                    '()])
          (let* ([port (open-input-file legacy-path)]
                 [data (read port)])
            (close-input-port port)
            (if (list? data)
                (let ([hash (rlm2-store-memory-block! data #f)])
                  (rlm2-write-memory-head! hash)
                  (set! *rlm2-system-memory-head* hash)
                  (format (current-error-port)
                    "[RLM] Migrated ~a memories from legacy file to CAS (~a)~%"
                    (length data) (hash->hex hash))
                  data)
                '())))
        '())))

(define (rlm2-load-system-memory!)
  (or *rlm2-system-memory-cache*
      (let ([head (rlm2-read-memory-head)])
        (if head
            ;; Load from CAS (fetch-persistent checks disk on cache miss)
            (let ([blk (fetch-persistent head)])
              (if blk
                  (let ([entries (rlm2-memory-block->entries blk)])
                    (set! *rlm2-system-memory-cache* entries)
                    (set! *rlm2-system-memory-head* head)
                    entries)
                  ;; Head exists but block missing — corrupt state
                  (begin
                    (format (current-error-port)
                      "[RLM] WARNING: memory head points to missing block ~a~%"
                      (hash->hex head))
                    (set! *rlm2-system-memory-cache* '())
                    '())))
            ;; No head — try legacy migration, else start empty
            (let ([migrated (rlm2-migrate-legacy-memory!)])
              (if (pair? migrated)
                  (begin (set! *rlm2-system-memory-cache* migrated) migrated)
                  (begin (set! *rlm2-system-memory-cache* '()) '())))))))

(define (rlm2-invalidate-system-memory-cache!)
  (set! *rlm2-system-memory-cache* #f)
  (set! *rlm2-system-memory-index* #f)
  (set! *rlm2-system-memory-head* #f))

(define (rlm2-append-system-memory! key text)
  (let* ([memories (rlm2-load-system-memory!)]
         [entry (list key text (rlm2-current-iso8601))]
         [updated (append memories (list entry))]
         [hash (rlm2-store-memory-block! updated *rlm2-system-memory-head*)])
    (rlm2-write-memory-head! hash)
    ;; Update caches
    (set! *rlm2-system-memory-cache* updated)
    (set! *rlm2-system-memory-head* hash)
    (when *rlm2-system-memory-index*
      (let* ([doc-id (- (length updated) 1)]
             [doc-text (format "~a ~a" key text)]
             [terms (tokenize doc-text)])
        (set! *rlm2-system-memory-index*
              (bm25-add-doc *rlm2-system-memory-index*
                            doc-id terms entry))))))

(define (rlm2-build-system-memory-index!)
  (or *rlm2-system-memory-index*
      (let* ([memories (rlm2-load-system-memory!)]
             [idx0 (bm25-create)])
        (let loop ([ms memories] [i 0] [idx idx0])
          (if (null? ms)
              (begin (set! *rlm2-system-memory-index* idx) idx)
              (let* ([entry (car ms)]
                     [text (format "~a ~a" (car entry) (cadr entry))]
                     [terms (tokenize text)]
                     [idx* (bm25-add-doc idx i terms entry)])
                (loop (cdr ms) (+ i 1) idx*)))))))

(define (rlm2-exec-memorize state action)
  (let ([key (rlm2-memorize-key action)]
        [text (rlm2-memorize-text action)])
    (if (not (string? text))
        (list (make-rlm2-observation 'memorize key
                (rlm2-format-diagnostic
                  (rlm2-error-type-mismatch "string" text "memorize text"))
                #f)
              state 1)
        (begin
          (rlm2-append-system-memory! key text)
          (list (make-rlm2-observation 'memorize key
                  (format "Saved to persistent memory under '~a'" key) #t)
                state 1)))))

(define (rlm2-exec-remember state action)
  (let ([query (rlm2-remember-query action)])
    (if (not (string? query))
        (list (make-rlm2-observation 'remember query
                (rlm2-format-diagnostic
                  (rlm2-error-type-mismatch "string" query "remember query"))
                #f)
              state 1)
        (let ([memories (rlm2-load-system-memory!)])
          (if (null? memories)
              (list (make-rlm2-observation 'remember query
                      "No memories stored yet." #t)
                    state 1)
              (let* ([idx (rlm2-build-system-memory-index!)]
                     [results (bm25-search-string idx query 3)]
                     [entries (map (lambda (r)
                                    (let ([data (bm25-get-data idx (car r))])
                                      (if data
                                          (format "(~a ~s ~a)"
                                                  (car data) (cadr data)
                                                  (if (>= (length data) 3) (caddr data) ""))
                                          (format "match:~a" (car r)))))
                                  results)])
                (list (make-rlm2-observation 'remember query
                        (if (null? entries)
                            "No matching memories found."
                            (rlm2-join-journal-entries entries))
                        #t)
                      state 1)))))))

