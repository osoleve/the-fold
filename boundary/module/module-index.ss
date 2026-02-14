(define-syntax doc
  (syntax-rules ()
    [(_ args ...) (void)]))

(doc 'module 'module/module-index)
(doc 'description "Module index management: builds and manages the module lookup hashtable from manifests")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(boundary/module/manifest-scanner lattice/meta/manifest))
(doc 'note "This is the glue between manifest discovery and the core module system")

(doc 'section 'state)

(doc *manifest-simple-index* 'type (Hashtable Symbol String))
(doc *manifest-simple-index* 'description "Maps module names to file paths, populated by module-index-init!")
(define *manifest-simple-index* (make-eq-hashtable))

(doc *manifest-namespaced-idx* 'type (Hashtable Symbol String))
(doc *manifest-namespaced-idx* 'description "Maps namespaced module names (skill/module) to file paths, always unambiguous")
(define *manifest-namespaced-idx* (make-eq-hashtable))

(doc *manifest-index-initialized* 'type Boolean)
(doc *manifest-index-initialized* 'description "Track whether the index has been built")
(define *manifest-index-initialized* #f)

(doc 'section 'index-building)

(define (module-index-build!)
  (doc 'type (-> Stats))
  (doc 'description "Scan all manifests and build the module index, must be called after manifest-scanner.ss and manifest.ss are loaded")
  (doc 'export #t)
  ;; Clear existing index
  (hashtable-clear! *manifest-simple-index*)
  (hashtable-clear! *manifest-namespaced-idx*)

  ;; Scan all manifests
  (let* ([manifests (scan-all-manifests)]
         [simple-count 0]
         [namespaced-count 0]
         [collision-count 0])

        ;; Process each manifest
        (for-each
         (lambda (path-sexp)
                 (let* ([sexp (cdr path-sexp)]
                        [manifest (parse-manifest sexp)])
                       (when (and manifest (valid-manifest? manifest))
                             ;; Build simple index (may have collisions)
                             (let ([simple-entries (manifest->module-index manifest)])
                                  (for-each
                                   (lambda (entry)
                                           (let ([name (car entry)]
                                                 [path (cdr entry)])
                                                ;; Track collisions but don't overwrite
                                                (if (hashtable-contains? *manifest-simple-index* name)
                                                    (set! collision-count (+ collision-count 1))
                                                    (begin
                                                     (hashtable-set! *manifest-simple-index* name path)
                                                     (set! simple-count (+ simple-count 1))))))
                                   simple-entries))
                             ;; Build namespaced index (always unique)
                             (let ([namespaced-entries (manifest->namespaced-index manifest)])
                                  (for-each
                                   (lambda (entry)
                                           (let ([name (car entry)]
                                                 [path (cdr entry)])
                                                (hashtable-set! *manifest-namespaced-idx* name path)
                                                (set! namespaced-count (+ namespaced-count 1))))
                                   namespaced-entries)))))
         manifests)

        (set! *manifest-index-initialized* #t)

        ;; Return stats
        `((manifests . ,(length manifests))
          (simple-modules . ,simple-count)
          (namespaced-modules . ,namespaced-count)
          (collisions-skipped . ,collision-count))))

(doc 'section 'lookup)

(define (module-index-lookup name)
  (doc 'type (-> Symbol (Maybe String)))
  (doc 'description "Look up a module in the index, supports both simple names (vec) and namespaced names (linalg/vec)")
  (doc 'export #t)
  (let ([name-str (symbol->string name)])
       (if (module-index-string-contains? name-str "/")
           ;; Namespaced: check namespaced index only
           (hashtable-ref *manifest-namespaced-idx* name #f)
           ;; Simple: check simple index first, then namespaced as fallback
           (or (hashtable-ref *manifest-simple-index* name #f)
               (hashtable-ref *manifest-namespaced-idx* name #f)))))

(define (module-index-string-contains? haystack needle)
  (doc 'type (-> String String Bool))
  (doc 'description "Check if string contains substring")
  (let ([h-len (string-length haystack)]
        [n-len (string-length needle)])
       (let loop ([i 0])
            (cond
             [(> (+ i n-len) h-len) #f]
             [(string=? (substring haystack i (+ i n-len)) needle) #t]
             [else (loop (+ i 1))]))))

(doc 'section 'core-module-system-integration)

(define (module-index-inject!)
  (doc 'type (-> Void))
  (doc 'description "Inject the module index into core/lang/module.ss, registers the index with the core module system")
  (doc 'export #t)
  (when (and *manifest-index-initialized*
             (top-level-bound? 'register-manifest-index!))
        (register-manifest-index! *manifest-simple-index* *manifest-namespaced-idx*)))

(doc 'section 'initialization)

(define (module-index-init!)
  (doc 'type (-> Stats))
  (doc 'description "Full initialization: build index and inject into core")
  (doc 'export #t)
  (let ([stats (module-index-build!)])
       (module-index-inject!)
       stats))

(define (module-index-initialized?)
  (doc 'type (-> Boolean))
  (doc 'description "Check if index has been initialized")
  (doc 'export #t)
  *manifest-index-initialized*)

(doc 'section 'diagnostics)

(define (module-index-stats)
  (doc 'type (-> Void))
  (doc 'description "Display module index statistics")
  (doc 'export #t)
  (display "\n")
  (display "  Module Index Statistics\n")
  (display "  --------------------------------------------------------\n")
  (display (format "  Initialized: ~a\n" *manifest-index-initialized*))
  (display (format "  Simple modules: ~a\n" (hashtable-size *manifest-simple-index*)))
  (display (format "  Namespaced modules: ~a\n" (hashtable-size *manifest-namespaced-idx*)))
  (display "\n"))

(define (module-index-list)
  (doc 'type (-> (List Symbol)))
  (doc 'description "List all modules in the simple index (sorted)")
  (doc 'export #t)
  (sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
        (vector->list (hashtable-keys *manifest-simple-index*))))

(define (module-index-list-namespaced)
  (doc 'type (-> (List Symbol)))
  (doc 'description "List all modules in the namespaced index (sorted)")
  (doc 'export #t)
  (sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
        (vector->list (hashtable-keys *manifest-namespaced-idx*))))

(define (module-index-find-collisions)
  (doc 'type (-> (List Symbol)))
  (doc 'description "Find module names that exist in multiple skills, returns names that would collide if we didn't use first-match")
  (doc 'export #t)
  ;; Re-scan to find all paths for each name
  (let ([all-names (make-eq-hashtable)]
        [manifests (scan-all-manifests)])
       (for-each
        (lambda (path-sexp)
                (let* ([sexp (cdr path-sexp)]
                       [manifest (parse-manifest sexp)])
                      (when (and manifest (valid-manifest? manifest))
                            (let ([entries (manifest->module-index manifest)])
                                 (for-each
                                  (lambda (entry)
                                          (let* ([name (car entry)]
                                                 [existing (hashtable-ref all-names name '())])
                                                (hashtable-set! all-names name
                                                                (cons (cdr entry) existing))))
                                  entries)))))
        manifests)
       ;; Find names with multiple paths
       (filter (lambda (name)
                       (> (length (hashtable-ref all-names name '())) 1))
               (vector->list (hashtable-keys all-names)))))
