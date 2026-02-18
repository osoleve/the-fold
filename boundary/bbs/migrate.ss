(doc 'module 'boundary/bbs/migrate)
(doc 'description "BBS Migration - One-time migration from flat layout to multi-board layout")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Auto-detected on init: if .bbs/boards.sexp is absent but .bbs/counter exists,
run migration. Idempotent: safe to run multiple times.")

(doc 'section 'migration-detection)

(define (bbs-needs-migration?)
  (doc 'type '(-> Boolean))
  (doc 'description "Check if the BBS needs migration from flat to multi-board layout")
  (doc 'export #t)
  (and (not (file-exists? ".bbs/boards.sexp"))
       ;; Only migrate if there's actual BBS data (counter or heads)
       (or (file-exists? ".bbs/counter")
           (file-exists? ".bbs/post-counter")
           (file-exists? ".store/heads/bbs"))))

(doc 'section 'migration)

(define (bbs-migrate-to-boards!)
  (doc 'type '(-> Void))
  (doc 'description "Migrate from flat BBS layout to multi-board layout. Idempotent.")
  (doc 'export #t)
  (printf "Migrating BBS to multi-board layout...~n")

  ;; 1. Create board directories
  (ensure-dir! ".bbs/boards")
  (ensure-dir! ".bbs/boards/issues")
  (ensure-dir! ".bbs/boards/changelog")
  (ensure-dir! ".store/heads/bbs/issues")
  (ensure-dir! ".store/heads/bbs/changelog")

  ;; 2. Move issue metadata
  (migrate-file! ".bbs/counter" ".bbs/boards/issues/counter")
  (migrate-file! ".bbs/deps" ".bbs/boards/issues/deps")
  (migrate-file! ".bbs/index.cache" ".bbs/boards/issues/index.cache")

  ;; 3. Move post metadata
  (migrate-file! ".bbs/post-counter" ".bbs/boards/changelog/counter")
  (migrate-file! ".bbs/post-index.cache" ".bbs/boards/changelog/index.cache")

  ;; 4. Move issue head files (fold-*.head -> issues/)
  (let ([heads-dir ".store/heads/bbs"])
    (when (file-exists? heads-dir)
      (let ([entries (directory-list heads-dir)])
        (for-each
         (lambda (entry)
           (when (and (> (string-length entry) 5)
                      (string=? (substring entry (- (string-length entry) 5)
                                           (string-length entry))
                                ".head"))
             (let ([id (substring entry 0 (- (string-length entry) 5))])
               (cond
                ;; Issue heads -> issues/
                [(and (>= (string-length id) 5)
                      (string=? (substring id 0 5) "fold-"))
                 (migrate-file! (string-append heads-dir "/" entry)
                                (string-append heads-dir "/issues/" entry))]
                ;; Post heads -> changelog/
                [(and (>= (string-length id) 5)
                      (string=? (substring id 0 5) "post-"))
                 (migrate-file! (string-append heads-dir "/" entry)
                                (string-append heads-dir "/changelog/" entry))]
                ;; Skip directories and unknown files
                [else #f]))))
         entries))))

  ;; 5. Write board descriptors
  (write-sexp-file! ".bbs/boards/issues/board.sexp"
    '((name . "issues")
      (slug . "issues")
      (board-type . tracker)
      (id-prefix . "fold-")
      (description . "The Fold issue tracker")))

  (write-sexp-file! ".bbs/boards/changelog/board.sexp"
    '((name . "changelog")
      (slug . "changelog")
      (board-type . feed)
      (id-prefix . "post-")
      (description . "Changelog and announcements")))

  ;; 6. Write board registry
  (write-sexp-file! ".bbs/boards.sexp"
    '(("issues" tracker "fold-" "The Fold issue tracker")
      ("changelog" feed "post-" "Changelog and announcements")))

  (printf "Migration complete.~n")
  (printf "  issues -> .bbs/boards/issues/ + .store/heads/bbs/issues/~n")
  (printf "  posts  -> .bbs/boards/changelog/ + .store/heads/bbs/changelog/~n"))

(doc 'section 'migration-helpers)

(define (ensure-dir! path)
  (doc 'type '(-> String Void))
  (doc 'description "Ensure a directory exists, creating parents as needed.
Splits on '/' and creates each level if absent.")
  (unless (file-exists? path)
    ;; Build parent chain by splitting on /
    (let loop ([parts (let split ([s path] [acc '()])
                        (let ([idx (let find ([i 0])
                                     (cond
                                      [(>= i (string-length s)) #f]
                                      [(char=? (string-ref s i) #\/) i]
                                      [else (find (+ i 1))]))])
                          (if idx
                              (split (substring s (+ idx 1) (string-length s))
                                     (cons (substring s 0 idx) acc))
                              (reverse (cons s acc)))))]
               [prefix ""])
      (unless (null? parts)
        (let ([dir (if (string=? prefix "")
                       (car parts)
                       (string-append prefix "/" (car parts)))])
          (unless (or (string=? dir "") (file-exists? dir))
            (mkdir dir))
          (loop (cdr parts) dir))))))

(define (migrate-file! src dst)
  (doc 'type '(-> String String Void))
  (doc 'description "Move a file from src to dst if src exists and dst doesn't")
  (when (and (file-exists? src)
             (not (file-exists? dst)))
    ;; Copy content then delete source
    (let ([content (call-with-port
                    (open-file-input-port src)
                    (lambda (port) (get-bytevector-all port)))])
      (call-with-port
       (open-file-output-port dst (file-options no-fail))
       (lambda (port) (put-bytevector port content)))
      (delete-file src))))

(define (write-sexp-file! path data)
  (doc 'type '(-> String Any Void))
  (doc 'description "Write an S-expression to a file")
  (call-with-output-file path
    (lambda (port)
      (write data port)
      (newline port))
    '(replace)))
