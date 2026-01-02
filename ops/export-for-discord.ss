;;; scripts/export-for-discord.ss — Export forum posts for Discord seeding
;;;
;;; Exports all forum posts to JSON format for importing into Discord.
;;; Posts are ordered oldest-first for chronological seeding.
;;;
;;; Usage:
;;;   ./fold.sh "(load \"scripts/export-for-discord.ss\")"
;;;   ./fold.sh "(export-all-channels \"./exports\")"

;;; ============================================================
;;; Dependencies
;;; ============================================================

(load "core/blocks/block.ss")
(load "core/base/sha256.ss")
(load "shell/fs.ss")
(load "forum/tools.ss")

;;; ============================================================
;;; JSON Serialization (minimal, for export)
;;; ============================================================

;;; escape-json-string : String → String
;;; Escape special characters for JSON string.
(define (escape-json-string s)
  (let loop ([chars (string->list s)]
             [result '()])
       (if (null? chars)
           (list->string (reverse result))
           (let ([c (car chars)])
                (cond
                 [(char=? c #\") (loop (cdr chars) (append '(#\" #\\) result))]
                 [(char=? c #\\) (loop (cdr chars) (append '(#\\ #\\) result))]
                 [(char=? c #\newline) (loop (cdr chars) (append '(#\n #\\) result))]
                 [(char=? c #\return) (loop (cdr chars) (append '(#\r #\\) result))]
                 [(char=? c #\tab) (loop (cdr chars) (append '(#\t #\\) result))]
                 [else (loop (cdr chars) (cons c result))])))))

;;; value->json : Any → String
;;; Convert a Scheme value to JSON string.
(define (value->json v)
  (cond
   [(string? v) (string-append "\"" (escape-json-string v) "\"")]
   [(symbol? v) (string-append "\"" (symbol->string v) "\"")]
   [(number? v) (number->string v)]
   [(boolean? v) (if v "true" "false")]
   [(null? v) "null"]
   [(list? v)
    (if (and (pair? v) (pair? (car v)) (symbol? (caar v)))
        ;; Alist → JSON object
        (string-append "{"
                       (apply string-append
                              (let loop ([items v] [first? #t])
                                   (if (null? items)
                                       '()
                                       (let* ([item (car items)]
                                              [key (car item)]
                                              [val (cdr item)]
                                              [prefix (if first? "" ",")])
                                             (cons (string-append prefix
                                                                  "\"" (symbol->string key) "\":"
                                                                  (value->json val))
                                                   (loop (cdr items) #f))))))
                       "}")
        ;; Regular list → JSON array
        (string-append "["
                       (apply string-append
                              (let loop ([items v] [first? #t])
                                   (if (null? items)
                                       '()
                                       (let ([prefix (if first? "" ",")])
                                            (cons (string-append prefix (value->json (car items)))
                                                  (loop (cdr items) #f))))))
                       "]"))]
   [else "null"]))

;;; ============================================================
;;; Post Export
;;; ============================================================

;;; post->discord-format : Bytevector × Alist → Alist
;;; Convert a forum post to export format with hash.
(define (post->discord-format hash post)
  (let ([author (cdr (assq 'author post))]
        [tier (cdr (assq 'tier post))]
        [timestamp (cdr (assq 'timestamp post))]
        [channel (cdr (assq 'channel post))]
        [body (cdr (assq 'body post))]
        [title-pair (assq 'title post)]
        [parent-pair (assq 'parent post)]
        [parent-hash-pair (assq 'parent-hash post)]
        [tags-pair (assq 'tags post)])
       `((hash . ,(hash->hex hash))
         (author . ,author)
         (tier . ,tier)
         (timestamp . ,timestamp)
         (channel . ,channel)
         (title . ,(if title-pair (cdr title-pair) #f))
         (body . ,body)
         (parent . ,(cond
                     [parent-hash-pair (cdr parent-hash-pair)]
                     [parent-pair (cdr parent-pair)]
                     [else #f]))
         (tags . ,(if tags-pair (cdr tags-pair) '())))))

;;; export-channel-for-discord : FS × Symbol → (List Alist)
;;; Export all posts from a channel, oldest first, in export format.
(define (export-channel-for-discord fs channel)
  (let* ([posts-with-hash (collect-channel-with-hash fs channel)]
         [formatted (map (lambda (p) (post->discord-format (car p) (cdr p)))
                         posts-with-hash)])
        ;; collect-channel-with-hash returns newest first, reverse for oldest first
        (reverse formatted)))

;;; ============================================================
;;; File Export
;;; ============================================================

;;; ensure-directory : String → void
;;; Create directory if it doesn't exist.
(define (ensure-directory path)
  (unless (file-exists? path)
          (mkdir path)))

;;; export-channel-to-file : FS × Symbol × String → void
;;; Export a single channel to a JSON file.
(define (export-channel-to-file fs channel output-dir)
  (let* ([posts (export-channel-for-discord fs channel)]
         [json-array (value->json posts)]
         [path (string-append output-dir "/" (symbol->string channel) ".json")])
        (call-with-output-file path
                               (lambda (port)
                                       (put-string port json-array)))
        (display (format "Exported ~a posts from #~a to ~a\n"
                         (length posts) channel path))))

;;; export-all-channels : String → void
;;; Export all forum channels to JSON files in the given directory.
(define (export-all-channels output-dir)
  (let* ([fs (mint-fs-capability ".store")]
         [channels (list-channels fs)])
        (ensure-directory output-dir)
        (display (format "Found ~a channels: ~a\n" (length channels) channels))
        (for-each
         (lambda (channel)
                 (export-channel-to-file fs channel output-dir))
         channels)
        (display "\nExport complete!\n")))

;;; ============================================================
;;; Summary Statistics
;;; ============================================================

;;; export-summary : String → void
;;; Print summary of exported data.
(define (export-summary output-dir)
  (let* ([fs (mint-fs-capability ".store")]
         [channels (list-channels fs)])
        (display "\n=== Forum Export Summary ===\n\n")
        (let ([total 0])
             (for-each
              (lambda (channel)
                      (let* ([posts (collect-channel fs channel)]
                             [count (length posts)])
                            (set! total (+ total count))
                            (display (format "  #~a: ~a posts\n" channel count))))
              channels)
             (display (format "\n  Total: ~a posts\n\n" total)))))

;;; ============================================================
;;; Interactive Helpers
;;; ============================================================

(display "\n=== Discord Export Script Loaded ===\n\n")
(display "Commands:\n")
(display "  (export-summary \"./exports\")     - Show post counts per channel\n")
(display "  (export-all-channels \"./exports\") - Export all channels to JSON\n\n")

;; Auto-run summary on load
(export-summary "./exports")
