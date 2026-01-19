;;; boundary/module/module-index.ss — Module Index Management
;;;
;;; Builds and manages the module lookup hashtable from manifests.
;;; This is the glue between manifest discovery and the core module system.
;;;
;;; This is Shell code: coordinates I/O and state.
;;;
;;; Usage:
;;;   (module-index-init!)            ; Scan manifests, build index
;;;   (module-index-lookup name)      ; O(1) lookup → path | #f
;;;   (module-index-inject!)          ; Inject into core/lang/module.ss
;;;   (module-index-stats)            ; Show statistics
;;;
;;; Dependencies:
;;;   boundary/module/manifest-scanner.ss (I/O)
;;;   lattice/meta/manifest.ss (pure parsing)

;;; ====
;;; State
;;; ====

;;; *manifest-simple-index* : Hashtable Symbol → String
;;; Maps module names to file paths.
;;; Populated by module-index-init!
(define *manifest-simple-index* (make-eq-hashtable))

;;; *manifest-namespaced-idx* : Hashtable Symbol → String
;;; Maps namespaced module names (skill/module) to file paths.
;;; Always unambiguous.
(define *manifest-namespaced-idx* (make-eq-hashtable))

;;; *manifest-index-initialized* : Boolean
;;; Track whether the index has been built.
(define *manifest-index-initialized* #f)

;;; ====
;;; Index Building
;;; ====

;;; module-index-build! : -> Void
;;; Scan all manifests and build the module index.
;;; Must be called after manifest-scanner.ss and manifest.ss are loaded.
(define (module-index-build!)
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

;;; ====
;;; Lookup
;;; ====

;;; module-index-lookup : Symbol -> String | #f
;;; Look up a module in the index.
;;; Supports both simple names (vec) and namespaced names (linalg/vec).
(define (module-index-lookup name)
  (let ([name-str (symbol->string name)])
       (if (module-index-string-contains? name-str "/")
           ;; Namespaced: check namespaced index only
           (hashtable-ref *manifest-namespaced-idx* name #f)
           ;; Simple: check simple index first, then namespaced as fallback
           (or (hashtable-ref *manifest-simple-index* name #f)
               (hashtable-ref *manifest-namespaced-idx* name #f)))))

;;; module-index-string-contains? : String String -> Bool
(define (module-index-string-contains? haystack needle)
  (let ([h-len (string-length haystack)]
        [n-len (string-length needle)])
       (let loop ([i 0])
            (cond
             [(> (+ i n-len) h-len) #f]
             [(string=? (substring haystack i (+ i n-len)) needle) #t]
             [else (loop (+ i 1))]))))

;;; ====
;;; Core Module System Integration
;;; ====

;;; module-index-inject! : -> Void
;;; Inject the module index into core/lang/module.ss.
;;; This registers the index with the core module system.
(define (module-index-inject!)
  (when (and *manifest-index-initialized*
             (top-level-bound? 'register-manifest-index!))
        (register-manifest-index! *manifest-simple-index* *manifest-namespaced-idx*)))

;;; ====
;;; Initialization
;;; ====

;;; module-index-init! : -> Stats
;;; Full initialization: build index and inject into core.
(define (module-index-init!)
  (let ([stats (module-index-build!)])
       (module-index-inject!)
       stats))

;;; module-index-initialized? : -> Boolean
(define (module-index-initialized?)
  *manifest-index-initialized*)

;;; ====
;;; Diagnostics
;;; ====

;;; module-index-stats : -> Void
;;; Display module index statistics.
(define (module-index-stats)
  (display "\n")
  (display "  Module Index Statistics\n")
  (display "  ────────────────────────────────────────────────────────\n")
  (display (format "  Initialized: ~a\n" *manifest-index-initialized*))
  (display (format "  Simple modules: ~a\n" (hashtable-size *manifest-simple-index*)))
  (display (format "  Namespaced modules: ~a\n" (hashtable-size *manifest-namespaced-idx*)))
  (display "\n"))

;;; module-index-list : -> (List Symbol)
;;; List all modules in the simple index (sorted).
(define (module-index-list)
  (sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
        (vector->list (hashtable-keys *manifest-simple-index*))))

;;; module-index-list-namespaced : -> (List Symbol)
;;; List all modules in the namespaced index (sorted).
(define (module-index-list-namespaced)
  (sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
        (vector->list (hashtable-keys *manifest-namespaced-idx*))))

;;; module-index-find-collisions : -> (List Symbol)
;;; Find module names that exist in multiple skills.
;;; Returns names that would collide if we didn't use first-match.
(define (module-index-find-collisions)
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
