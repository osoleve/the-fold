;;; boundary/capability/fold-cap.ss — Fold Capability Layer
;;;
;;; Shared env/memory operations for MCP and RLM agents.
;;; Workers load this on demand to provide CAS-backed environment
;;; and persistent memory capabilities.
;;;
;;; This module is loaded lazily by the worker on first env/memory command.
;;; It depends on rlm-env.ss (CAS operations) and cas-persist.ss (persistence).

(load "boundary/pipeline/rlm-env.ss")
(load "boundary/io/atomic.ss")

(doc 'module 'fold-cap)
(doc 'description "Capability layer: per-session env/memory operations for MCP and RLM agents")
(doc 'layer 'boundary)
(doc 'purity 'impure)

;;; ====
;;; Per-Session Environment State
;;; ====
;;;
;;; Each session gets its own env namespace. The env is a standard
;;; rlm-env alist: ((key type size hash) ...)
;;; Env-mutating operations return an env summary alongside the result
;;; so callers always have current state without extra round-trips.

(define *session-envs* (make-hashtable string-hash string=?))

(define (fold-cap-get-env session-id)
  (or (hashtable-ref *session-envs* session-id #f)
      (make-rlm-env)))

(define (fold-cap-set-env! session-id env)
  (hashtable-set! *session-envs* session-id env))

;;; ====
;;; Env Summary (returned with every mutation)
;;; ====

(define (fold-cap-env-summary env)
  (map (lambda (entry)
         (list (car entry)     ; key
               (cadr entry)    ; type
               (caddr entry))) ; size
       env))

;;; ====
;;; Env Operations
;;; ====

;;; fold-cap-env-store! : String × Symbol × Any × Symbol → (value . env-summary)
(define (fold-cap-env-store! session-id key value tag)
  (let* ([env (fold-cap-get-env session-id)]
         [result (rlm-env-store! env key value tag)]
         [env* (car result)]
         [hex (cdr result)])
    (fold-cap-set-env! session-id env*)
    (cons (format "Stored as '~a' (~a)" key hex)
          (fold-cap-env-summary env*))))

;;; fold-cap-env-fetch : String × Symbol → Any | #f
(define (fold-cap-env-fetch session-id key)
  (let ([env (fold-cap-get-env session-id)])
    (rlm-env-fetch env key)))

;;; fold-cap-env-peek : String × Symbol × Nat → String | #f
(define (fold-cap-env-peek session-id key n)
  (let ([env (fold-cap-get-env session-id)])
    (rlm-env-peek env key n)))

;;; fold-cap-env-grep : String × Symbol × String × Nat → (List (String . Number)) | #f
(define (fold-cap-env-grep session-id key pattern k)
  (let ([env (fold-cap-get-env session-id)])
    (rlm-env-grep env key pattern k)))

;;; fold-cap-env-slice : String × Symbol × Nat × Nat → String
(define (fold-cap-env-slice session-id key start end)
  (let ([env (fold-cap-get-env session-id)])
    (let loop ([idx start] [acc '()])
      (if (>= idx end)
          (apply string-append (reverse acc))
          (let ([chunk (rlm-env-read-chunk env key idx)])
            (if chunk
                (loop (+ idx 1) (cons chunk acc))
                (loop (+ idx 1) acc)))))))

;;; fold-cap-env-ingest! : String × Symbol × String × Nat → env-summary
(define (fold-cap-env-ingest! session-id key text chunk-size)
  (let* ([env (fold-cap-get-env session-id)]
         [env* (rlm-env-ingest-text! env key text chunk-size)])
    (fold-cap-set-env! session-id env*)
    (fold-cap-env-summary env*)))

;;; fold-cap-env-keys : String → (List (Symbol Symbol Nat))
(define (fold-cap-env-keys session-id)
  (let ([env (fold-cap-get-env session-id)])
    (rlm-env-keys env)))

;;; fold-cap-env-map-chunks : String × Symbol × String → (List Any)
;;; Evaluate expr per chunk with *chunk* bound. Returns list of results.
;;; The expression is compiled once as a lambda and applied per chunk.
(define (fold-cap-env-map-chunks session-id key expr-text)
  (let* ([env (fold-cap-get-env session-id)]
         [manifest (rlm-env-fetch env key)])
    (if (not (and manifest (chunk-manifest? manifest)))
        '()
        ;; Compile once: (lambda (*chunk*) <expr>)
        (let ([proc (guard (ex [else #f])
                      (eval (read (open-input-string
                                    (format "(lambda (*chunk*) ~a)" expr-text)))))])
          (if (not proc)
              ;; Parse/compile failed — return error for every chunk
              (map (lambda (_) (format "error:invalid expression: ~a" expr-text))
                   (chunk-manifest-hashes manifest))
              (map (lambda (hash)
                     (let ([blk (fetch-persistent (hex->hash hash))])
                       (if blk
                           (guard (ex [else
                                       (format "error:~a"
                                               (if (message-condition? ex)
                                                   (condition-message ex) ex))])
                             (proc (utf8->string (block-payload blk))))
                           "")))
                   (chunk-manifest-hashes manifest)))))))

;;; ====
;;; Persistent Memory (cross-session)
;;; ====

(define *fold-cap-memory-cache* #f)
(define *fold-cap-memory-index* #f)

(define (fold-cap-memory-path)
  (string-append (current-directory) "/.rlm/memories.sexp"))

(define (fold-cap-ensure-rlm-dir!)
  (let ([dir (string-append (current-directory) "/.rlm")])
    (unless (file-exists? dir)
      (mkdir dir))))

(define (fold-cap-load-memory!)
  (or *fold-cap-memory-cache*
      (let ([path (fold-cap-memory-path)])
        (if (file-exists? path)
            (guard (ex [else
                        (set! *fold-cap-memory-cache* '())
                        '()])
              (let* ([port (open-input-file path)]
                     [data (read port)])
                (close-input-port port)
                (if (list? data)
                    (begin (set! *fold-cap-memory-cache* data) data)
                    (begin (set! *fold-cap-memory-cache* '()) '()))))
            (begin (set! *fold-cap-memory-cache* '()) '())))))

(define (fold-cap-build-memory-index!)
  (or *fold-cap-memory-index*
      (let* ([memories (fold-cap-load-memory!)]
             [idx0 (bm25-create)])
        (let loop ([ms memories] [i 0] [idx idx0])
          (if (null? ms)
              (begin (set! *fold-cap-memory-index* idx) idx)
              (let* ([entry (car ms)]
                     [text (format "~a ~a" (car entry) (cadr entry))]
                     [terms (tokenize text)]
                     [idx* (bm25-add-doc idx i terms entry)])
                (loop (cdr ms) (+ i 1) idx*)))))))

;;; fold-cap-memorize! : Symbol × String → void
;;; Re-reads from disk before appending to avoid stale-cache overwrites
;;; when multiple workers write to the same memories.sexp file.
(define (fold-cap-memorize! key text)
  (fold-cap-ensure-rlm-dir!)
  ;; Invalidate cache and re-read from disk to avoid overwriting
  ;; entries written by other workers since our last read.
  (set! *fold-cap-memory-cache* #f)
  (set! *fold-cap-memory-index* #f)
  (let* ([memories (fold-cap-load-memory!)]
         [ts (let ([d (current-date)])
               (format "~4d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
                       (date-year d) (date-month d) (date-day d)
                       (date-hour d) (date-minute d) (date-second d)))]
         [entry (list key text ts)]
         [updated (append memories (list entry))])
    (atomic-write-file (fold-cap-memory-path)
      (lambda (port)
        (write updated port)
        (newline port)))
    (set! *fold-cap-memory-cache* updated)
    ;; Incremental BM25 update
    (when *fold-cap-memory-index*
      (let* ([doc-id (- (length updated) 1)]
             [doc-text (format "~a ~a" key text)]
             [terms (tokenize doc-text)])
        (set! *fold-cap-memory-index*
              (bm25-add-doc *fold-cap-memory-index*
                            doc-id terms entry))))))

;;; fold-cap-remember : String × Nat → (List (Key Text Timestamp))
(define (fold-cap-remember query k)
  (let ([memories (fold-cap-load-memory!)])
    (if (null? memories)
        '()
        (let* ([idx (fold-cap-build-memory-index!)]
               [results (bm25-search-string idx query k)])
          (map (lambda (r)
                 (let ([data (bm25-get-data idx (car r))])
                   (or data (list 'unknown "" ""))))
               results)))))
