;;; boundary/pipeline/rlm-env.ss — CAS-Backed RLM Environment
;;;
;;; Impure operations for storing/fetching RLM environment values
;;; via the content-addressed store. Each value becomes a CAS block;
;;; the env alist maps keys to hashes.
;;;
;;; This is Shell code: handles IO, may fail, contains defensive logic.

(load "core/blocks/cas.ss")
(load "boundary/storage/cas-persist.ss")

;; meta-printf is defined in kg.ss but bm25.ss uses it at load time.
;; Provide a no-op stub to avoid pulling in the full knowledge graph.
(define (meta-printf . args) (void))
(load "lattice/meta/bm25.ss")

(doc 'module 'rlm-env)
(doc 'description "CAS-backed environment operations for RLM harness")
(doc 'layer 'boundary)
(doc 'purity 'impure)
(doc 'dependencies '(core/blocks/cas.ss boundary/storage/cas-persist.ss
                     lattice/meta/bm25.ss))

;;; ====
;;; RLM Environment Types (Pure)
;;; ====
;;;
;;; An RLM environment maps symbolic keys to metadata entries.
;;; Values themselves live in CAS — the env tracks what's available
;;; without materializing content.
;;;
;;; Entry: (key type size hash)
;;;   key  : Symbol — human-readable name
;;;   type : Symbol — 'text, 'sexpr, 'chunks, 'result, 'sub-result
;;;   size : Nat    — character count or item count
;;;   hash : String — CAS hash (hex) of the stored value

(doc 'type '(-> RlmEnv))
(doc 'description "Create an empty RLM environment")
(define (make-rlm-env) '())

(doc 'type '(-> RlmEnv Symbol Any Symbol RlmEnv))
(doc 'description "Add or update an entry in the environment.")
(define (rlm-env-put env key value-hash tag size)
  (let ([entry (list key tag size value-hash)])
    (cons entry (rlm-env-remove-key env key))))

(doc 'type '(-> RlmEnv Symbol (Maybe RlmEntry)))
(doc 'description "Look up an entry by key. Returns (key type size hash) or #f.")
(define (rlm-env-get env key)
  (let loop ([e env])
    (cond
      [(null? e) #f]
      [(eq? (caar e) key) (car e)]
      [else (loop (cdr e))])))

(doc 'type '(-> RlmEnv (List (List Symbol Symbol Nat))))
(doc 'description "List all keys with their types and sizes")
(define (rlm-env-keys env)
  (map (lambda (entry)
         (list (car entry)     ; key
               (cadr entry)    ; type
               (caddr entry))) ; size
       env))

(doc 'type '(-> RlmEnv Symbol RlmEnv))
(doc 'description "Remove a key from the environment")
(define (rlm-env-remove-key env key)
  (filter (lambda (entry) (not (eq? (car entry) key))) env))

(doc 'type '(-> RlmEnv String))
(doc 'description "Generate a human-readable summary of environment contents")
(define (rlm-env-summary env)
  (if (null? env)
      "[Environment is empty]"
      (let ([lines (map (lambda (entry)
                          (format "  ~a (~a, ~a chars)"
                                  (car entry)    ; key
                                  (cadr entry)   ; type
                                  (caddr entry))); size
                        env)])
        (string-append "Available context:\n"
                       (apply string-append
                              (map (lambda (l) (string-append l "\n")) lines))))))

(doc 'type '(-> RlmEnv Symbol String))
(doc 'description "Get the CAS hash for a key, or #f if not present")
(define (rlm-env-hash env key)
  (let ([entry (rlm-env-get env key)])
    (if entry (cadddr entry) #f)))

;;; ====
;;; Chunk Manifest Types
;;; ====

(doc 'type '(-> Symbol Nat Nat (List String) ChunkManifest))
(doc 'description "Metadata for a chunked text entry in the environment")
(define (make-chunk-manifest key total-chars chunk-count chunk-hashes)
  (list 'chunk-manifest key total-chars chunk-count chunk-hashes))

(define (chunk-manifest? x)
  (and (pair? x) (eq? (car x) 'chunk-manifest)))

(define (chunk-manifest-key m)          (list-ref m 1))
(define (chunk-manifest-total-chars m)  (list-ref m 2))
(define (chunk-manifest-count m)        (list-ref m 3))
(define (chunk-manifest-hashes m)       (list-ref m 4))

;;; ====
;;; Store / Fetch
;;; ====

;;; rlm-env-store! : RlmEnv -> Symbol -> Any -> Symbol -> (RlmEnv . String)
;;; Store a value in CAS and update the environment.
;;; Returns (env' . hex-hash).
(define (rlm-env-store! env key value tag)
  (let* ([blk (make-block 'rlm/value
                           (string->utf8 (format "~s" value))
                           (make-vector 0))]
         [hash (store-persistent! blk)]
         [hex (hash->hex hash)]
         [size (estimate-size value)]
         [env* (rlm-env-put env key hex tag size)])
    (cons env* hex)))

;;; rlm-env-fetch : RlmEnv -> Symbol -> Any | #f
;;; Retrieve a value from CAS by looking up its hash in the environment.
(define (rlm-env-fetch env key)
  (let ([entry (rlm-env-get env key)])
    (if (not entry)
        #f
        (let* ([hex (cadddr entry)]
               [hash (hex->hash hex)]
               [blk (fetch-persistent hash)])
          (if blk
              (guard (ex [else #f])
                (let ([str (utf8->string (block-payload blk))])
                  (read (open-input-string str))))
              #f)))))

;;; ====
;;; Text Ingestion (Chunked)
;;; ====

;;; rlm-env-ingest-text! : RlmEnv -> Symbol -> String -> Nat -> RlmEnv
;;; Split text into chunks, store each as a CAS block, create a manifest.
;;; The env key maps to the manifest.
(define (rlm-env-ingest-text! env key text chunk-size)
  (let* ([chunks (chunk-text text chunk-size)]
         [chunk-hashes (map (lambda (chunk)
                              (let* ([blk (make-block 'rlm/chunk
                                                       (string->utf8 chunk)
                                                       (make-vector 0))]
                                     [hash (store-persistent! blk)])
                                (hash->hex hash)))
                            chunks)]
         [manifest (make-chunk-manifest key
                                         (string-length text)
                                         (length chunks)
                                         chunk-hashes)]
         ;; Store the manifest itself
         [manifest-blk (make-block 'rlm/manifest
                                    (string->utf8 (format "~s" manifest))
                                    (make-vector 0))]
         [manifest-hash (store-persistent! manifest-blk)]
         [manifest-hex (hash->hex manifest-hash)]
         [env* (rlm-env-put env key manifest-hex 'chunks (string-length text))])
    env*))

;;; chunk-text : String -> Nat -> (List String)
;;; Split text into roughly equal chunks, breaking at newlines when possible.
(define (chunk-text text chunk-size)
  (let ([len (string-length text)])
    (if (<= len chunk-size)
        (list text)
        (let loop ([start 0] [chunks '()])
          (if (>= start len)
              (reverse chunks)
              (let* ([end (min (+ start chunk-size) len)]
                     ;; Try to break at a newline within the last 20% of the chunk
                     [break-point (find-break-point text start end)]
                     [chunk (substring text start break-point)])
                (loop break-point (cons chunk chunks))))))))

(define (find-break-point text start end)
  (let ([search-from (max start (- end (quotient (- end start) 5)))])
    (let loop ([i (- end 1)])
      (cond
        [(< i search-from) end]  ; no good break found, use hard end
        [(char=? (string-ref text i) #\newline) (+ i 1)]
        [else (loop (- i 1))]))))

;;; ====
;;; Peek (Partial Fetch)
;;; ====

;;; rlm-env-peek : RlmEnv -> Symbol -> Nat -> String | #f
;;; Return first n characters of a value without full materialization.
;;; For chunked values, reads only the first chunk(s) needed.
(define (rlm-env-peek env key n)
  (let ([entry (rlm-env-get env key)])
    (if (not entry)
        #f
        (let ([tag (cadr entry)])
          (case tag
            [(chunks)
             ;; Fetch manifest, then read chunks until we have n chars
             (let ([manifest (rlm-env-fetch env key)])
               (if (and manifest (chunk-manifest? manifest))
                   (peek-chunks (chunk-manifest-hashes manifest) n)
                   #f))]
            [else
             ;; Non-chunked: fetch full value and truncate
             (let ([value (rlm-env-fetch env key)])
               (if value
                   (let ([s (format "~a" value)])
                     (if (<= (string-length s) n)
                         s
                         (substring s 0 n)))
                   #f))])))))

(define (peek-chunks chunk-hashes n)
  (let loop ([hashes chunk-hashes] [collected ""] [remaining n])
    (cond
      [(<= remaining 0) collected]
      [(null? hashes) collected]
      [else
       (let* ([hash (hex->hash (car hashes))]
              [blk (fetch-persistent hash)])
         (if blk
             (let* ([text (utf8->string (block-payload blk))]
                    [take-len (min (string-length text) remaining)])
               (loop (cdr hashes)
                     (string-append collected (substring text 0 take-len))
                     (- remaining take-len)))
             collected))])))

;;; ====
;;; Grep (BM25 Search)
;;; ====

;;; Content-addressed BM25 index cache.
;;; Key: manifest CAS hex hash. Value: BM25 index.
;;; Same manifest hash = same chunks = same index, so this is
;;; always valid (CAS blocks are immutable). Built lazily on first grep.
(define *bm25-index-cache* (make-hashtable string-hash string=?))

;;; rlm-env-grep : RlmEnv -> Symbol -> String -> Nat -> (List (String . Number)) | #f
;;; Search chunks of a chunked env entry using BM25.
;;; Returns top-k list of (chunk-text . score), or #f if key not found/not chunked.
(define (rlm-env-grep env key pattern k)
  (let ([entry (rlm-env-get env key)])
    (if (not entry)
        #f
        (let ([tag (cadr entry)])
          (if (not (eq? tag 'chunks))
              #f  ; can only grep chunked values
              (let* ([manifest-hex (cadddr entry)]
                     [manifest (rlm-env-fetch env key)])
                (if (and manifest (chunk-manifest? manifest))
                    (let ([idx (get-or-build-bm25-index
                                 manifest-hex
                                 (chunk-manifest-hashes manifest))])
                      (search-bm25-index idx pattern k))
                    #f)))))))

;;; get-or-build-bm25-index : String -> (List String) -> BM25Index
;;; Returns cached index if available, otherwise builds and caches.
(define (get-or-build-bm25-index manifest-hex chunk-hashes)
  (or (hashtable-ref *bm25-index-cache* manifest-hex #f)
      (let ([idx (build-bm25-index chunk-hashes)])
        (hashtable-set! *bm25-index-cache* manifest-hex idx)
        idx)))

;;; build-bm25-index : (List String) -> BM25Index
(define (build-bm25-index chunk-hashes)
  (let loop ([hashes chunk-hashes]
             [idx (bm25-create)]
             [i 0])
    (if (null? hashes)
        idx
        (let* ([hash (hex->hash (car hashes))]
               [blk (fetch-persistent hash)])
          (if blk
              (let* ([text (utf8->string (block-payload blk))]
                     [terms (tokenize text)]
                     [idx* (bm25-add-doc idx i terms text)])
                (loop (cdr hashes) idx* (+ i 1)))
              (loop (cdr hashes) idx (+ i 1)))))))

;;; search-bm25-index : BM25Index -> String -> Nat -> (List (String . Number))
(define (search-bm25-index idx pattern k)
  (let* ([results (bm25-search-string idx pattern k)]
         [with-data (map (lambda (r)
                           (let ([data (bm25-get-data idx (car r))])
                             (cons (if data data "") (cdr r))))
                         results)])
    with-data))

;;; ====
;;; Chunk Access (Sequential)
;;; ====

;;; rlm-env-chunk-count : RlmEnv -> Symbol -> Nat | #f
;;; Return the number of chunks for a chunked entry, or #f if not chunked.
(define (rlm-env-chunk-count env key)
  (let ([entry (rlm-env-get env key)])
    (if (not entry)
        #f
        (let ([tag (cadr entry)])
          (if (not (eq? tag 'chunks))
              #f
              (let ([manifest (rlm-env-fetch env key)])
                (if (and manifest (chunk-manifest? manifest))
                    (chunk-manifest-count manifest)
                    #f)))))))

;;; rlm-env-read-chunk : RlmEnv -> Symbol -> Nat -> String | #f
;;; Read a specific chunk by index (0-based).
(define (rlm-env-read-chunk env key index)
  (let ([entry (rlm-env-get env key)])
    (if (not entry)
        #f
        (let ([tag (cadr entry)])
          (if (not (eq? tag 'chunks))
              #f
              (let ([manifest (rlm-env-fetch env key)])
                (if (and manifest (chunk-manifest? manifest))
                    (let ([hashes (chunk-manifest-hashes manifest)])
                      (if (and (>= index 0) (< index (length hashes)))
                          (let* ([hex (list-ref hashes index)]
                                 [hash (hex->hash hex)]
                                 [blk (fetch-persistent hash)])
                            (if blk
                                (utf8->string (block-payload blk))
                                #f))
                          #f))
                    #f)))))))

;;; ====
;;; Env Snapshot (for trajectory recording)
;;; ====

;;; rlm-env-snapshot! : RlmEnv -> String
;;; Store the current environment as a CAS block, return hex hash.
(define (rlm-env-snapshot! env)
  (let* ([keys-data (map (lambda (entry)
                           (list (car entry)     ; key
                                 (cadr entry)    ; type
                                 (caddr entry)   ; size
                                 (cadddr entry))); hash
                         env)]
         [snapshot `((keys . ,keys-data))]
         [blk (make-block 'rlm/env-snapshot
                           (string->utf8 (format "~s" snapshot))
                           ;; refs: all value hashes
                           (list->vector
                             (filter-map
                               (lambda (entry)
                                 (guard (ex [else #f])
                                   (hex->hash (cadddr entry))))
                               env)))]
         [hash (store-persistent! blk)])
    (hash->hex hash)))

;;; ====
;;; Utilities
;;; ====

(define (estimate-size value)
  (cond
    [(string? value) (string-length value)]
    [(pair? value)   (string-length (format "~s" value))]
    [else            (string-length (format "~a" value))]))

(define (filter-map f lst)
  (let loop ([l lst] [acc '()])
    (if (null? l)
        (reverse acc)
        (let ([v (f (car l))])
          (if v
              (loop (cdr l) (cons v acc))
              (loop (cdr l) acc))))))
