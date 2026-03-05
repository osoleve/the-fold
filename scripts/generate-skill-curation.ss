;;; scripts/generate-skill-curation.ss
;;; One-time migration: generate skills/*.sexp curation files from manifest.sexp files.
;;;
;;; Usage: scheme --script scripts/generate-skill-curation.ss [--dry-run]

(load "core/base/prelude.ss")
(load "lattice/meta/manifest.ss")
(load "boundary/meta/file-io.ss")

;;; ---- File utilities ----

(define (file-exists? path)
  (guard (exn [else #f])
    (let ([p (open-input-file path)]) (close-input-port p) #t)))

(define (ensure-directory! dir)
  (unless (file-exists? dir)
    (mkdir dir)))

(define (ensure-parent-dirs! path)
  ;; Create parent directories for a file path
  (let loop ([i 0] [last-sep -1])
    (cond
      [(>= i (string-length path)) (void)]
      [(char=? (string-ref path i) #\/)
       (let ([dir (substring path 0 i)])
         (when (and (> (string-length dir) 0) (not (file-exists? dir)))
           (mkdir dir)))
       (loop (+ i 1) i)]
      [else (loop (+ i 1) last-sep)])))

;;; ---- Extract module names from manifest ----

(define (extract-module-names manifest-data)
  (let* ([mods-entry (assq 'modules manifest-data)]
         [mods-raw (if (and mods-entry (list? (cdr mods-entry)))
                       (cdr mods-entry)
                       '())])
    (filter-map
     (lambda (m)
       (cond
         [(and (pair? m) (symbol? (car m))) (car m)]
         [else #f]))
     mods-raw)))

;;; ---- Format a curation sexp ----

(define (format-curation port skill-name manifest-data)
  (let* ([desc-entry (assq 'description manifest-data)]
         [desc (if (and desc-entry (string? (cdr desc-entry)))
                   (cdr desc-entry) "")]
         [keywords-entry (assq 'keywords manifest-data)]
         [keywords (if (and keywords-entry (list? (cdr keywords-entry)))
                       (cdr keywords-entry) '())]
         [aliases-entry (assq 'aliases manifest-data)]
         [aliases (if (and aliases-entry (list? (cdr aliases-entry)))
                      (cdr aliases-entry) '())]
         [concepts-entry (assq 'concepts manifest-data)]
         [concept-names (if (and concepts-entry (list? (cdr concepts-entry)))
                            (filter-map
                             (lambda (c)
                               (and (pair? c) (eq? (car c) 'concept)
                                    (pair? (cdr c)) (cadr c)))
                             (cdr concepts-entry))
                            '())]
         [module-names (extract-module-names manifest-data)])

    (fprintf port ";;; skills/~a.sexp — Skill curation file\n\n" skill-name)
    (fprintf port "(skill ~a\n" skill-name)

    ;; Description
    (when (and (string? desc) (> (string-length desc) 0))
      (fprintf port "  (description\n    ~s)\n\n" desc))

    ;; Keywords
    (when (pair? keywords)
      (fprintf port "  (keywords (~a))\n\n"
               (apply string-append
                      (let loop ([ks keywords] [acc '()])
                        (if (null? ks)
                            (reverse acc)
                            (loop (cdr ks)
                                  (cons (if (null? acc)
                                            (symbol->string (car ks))
                                            (string-append " " (symbol->string (car ks))))
                                        acc)))))))

    ;; Aliases
    (when (pair? aliases)
      (fprintf port "  (aliases (~a))\n\n"
               (apply string-append
                      (let loop ([as aliases] [acc '()])
                        (if (null? as)
                            (reverse acc)
                            (loop (cdr as)
                                  (cons (if (null? acc)
                                            (symbol->string (car as))
                                            (string-append " " (symbol->string (car as))))
                                        acc)))))))

    ;; Concepts (just names, definitions live in ontology.sexp)
    (when (pair? concept-names)
      (fprintf port "  (concepts (~a))\n\n"
               (apply string-append
                      (let loop ([cs concept-names] [acc '()])
                        (if (null? cs)
                            (reverse acc)
                            (loop (cdr cs)
                                  (cons (if (null? acc)
                                            (symbol->string (car cs))
                                            (string-append " " (symbol->string (car cs))))
                                        acc)))))))

    ;; Modules (just names — paths and descriptions live in source headers)
    (when (pair? module-names)
      (fprintf port "  (modules\n")
      (for-each
       (lambda (m) (fprintf port "    ~a\n" m))
       module-names)
      (fprintf port "  )\n\n"))

    ;; Prompts placeholder
    (fprintf port "  ;; Curated Q&A pairs for LLM context\n")
    (fprintf port "  (prompts)\n\n")

    ;; Suggested queries placeholder
    (fprintf port "  ;; KG/search queries an agent should try\n")
    (fprintf port "  (suggested-queries)\n")

    (fprintf port ")\n")))

;;; ---- Main ----

(define (main)
  (let* ([args (command-line-arguments)]
         [dry-run? (member "--dry-run" args)]
         [manifest-paths (find-manifests "lattice")]
         [total 0])
    (when dry-run? (display "=== DRY RUN MODE ===\n\n"))
    (printf "Found ~a manifests\n" (length manifest-paths))
    (ensure-directory! "skills")

    (for-each
     (lambda (manifest-path)
       (guard (e [else
                  (printf "  SKIP ~a: ~a\n" manifest-path
                          (if (message-condition? e) (condition-message e) e))])
         (let* ([sexp (read-manifest-sexp manifest-path)]
                [data (and sexp (parse-manifest sexp))]
                [skill-name (and data (cdr (assq 'name data)))]
                [out-path (and skill-name
                               (string-append "skills/"
                                              (symbol->string skill-name) ".sexp"))])
           (when (and data out-path)
             (if dry-run?
                 (printf "  DRY-RUN: ~a -> ~a (~a modules)\n"
                         manifest-path out-path
                         (length (extract-module-names data)))
                 (begin
                   (ensure-parent-dirs! out-path)
                   (call-with-output-file out-path
                     (lambda (port) (format-curation port skill-name data))
                     'replace)
                   (printf "  WROTE: ~a (~a modules)\n"
                           out-path (length (extract-module-names data)))))
             (set! total (+ total 1))))))
     manifest-paths)

    (printf "\n=== Total curation files: ~a ===\n" total)))

(main)
