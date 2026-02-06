(load "boundary/meta/file-io.ss")
(load "lattice/meta/kg.ss")

(doc 'module 'kg-io)
(doc 'description "I/O layer for knowledge graph — manifest discovery, reading, and build orchestration")
(doc 'layer 'boundary)

;;; kg-build! : -> Hash
;;; Build knowledge graph from all manifests in lattice/.
;;; Discovers manifest files on disk, reads them, and delegates to
;;; pure kg-add-skill! / kg-build-deps! in lattice/meta/kg.ss.
(define (kg-build!)
  (kg-reset!)
  (let ([manifests (find-manifests "lattice")])
       (printf "Found ~a manifests\n" (length manifests))
       (for-each
        (lambda (manifest-path)
                (let* ([sexp (read-manifest-sexp manifest-path)]
                       [data (if sexp (parse-manifest sexp) #f)])
                      (when data
                            (printf "  Loading: ~a\n" (cdr (assq 'name data)))
                            (kg-add-skill! data))))
        manifests)
       (kg-build-deps!)
       (let* ([skill-hashes (map (lambda (e) (hash-block (cdr e))) *kg-skills*)]
              [root (make-block KG-INDEX-ROOT
                                (string->utf8 (format "~a" (length *kg-skills*)))
                                (list->vector skill-hashes))])
             (set! *kg-index-root* (hash-block root))
             (set! *kg-loaded* #t)
             (printf "Knowledge graph built: ~a skills, ~a modules, ~a exports, ~a deps\n"
                     (length *kg-skills*)
                     (length *kg-modules*)
                     (length *kg-exports*)
                     (length *kg-deps*))
             *kg-index-root*)))

;;; kg-ensure! : -> Hash | #f
;;; Build knowledge graph only if not already initialized.
(define (kg-ensure!)
  (if (kg-initialized?)
      #f
      (kg-build!)))
