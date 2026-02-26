;;; @module module-reload
;;; @description Module reloading with dependency-aware topological ordering.
;;; Loaded by module.ss — requires module state tables and module-name->path.

;;; ====
;;; Module Reloading
;;; ====

(doc 'section 'reloading)

(doc *module-dependents* 'type (Hashtable Symbol (List Symbol)))
(doc *module-dependents* 'description "Reverse dependency index: maps module to list of modules that depend on it.")
(define *module-dependents* (make-eq-hashtable))

(doc build-dependents-index! 'type (-> Void))
(doc build-dependents-index! 'description "Rebuild the reverse dependency index from *module-deps*.")
(define (build-dependents-index!)
  (hashtable-clear! *module-dependents*)
  ;; For each module, add it to the dependents list of each of its deps
  (let ([modules (vector->list (hashtable-keys *module-deps*))])
    (for-each
     (lambda (mod)
       (let ([deps (hashtable-ref *module-deps* mod '())])
         (for-each
          (lambda (dep)
            (let ([existing (hashtable-ref *module-dependents* dep '())])
              (unless (memq mod existing)
                (hashtable-set! *module-dependents* dep (cons mod existing)))))
          deps)))
     modules)))

(doc module-dependents 'type (-> Symbol (List Symbol)))
(doc module-dependents 'description "Get direct dependents of a module (modules that depend on it).")
(define (module-dependents name)
  ;; Ensure index is built
  (when (zero? (hashtable-size *module-dependents*))
    (build-dependents-index!))
  (hashtable-ref *module-dependents* name '()))

(doc all-dependents 'type (-> Symbol (List Symbol)))
(doc all-dependents 'description "Get all transitive dependents of a module.")
(define (all-dependents name)
  (let ([direct (module-dependents name)])
    (if (null? direct)
        '()
        (unique (append direct (append-map all-dependents direct))))))

(doc topo-sort-reload 'type (-> (List Symbol) (List Symbol)))
(doc topo-sort-reload 'description "Topologically sort modules for reload: dependencies before dependents.")
(define (topo-sort-reload modules)
  ;; Kahn's algorithm: sort so deps come before dependents
  (let* ([mod-set (make-eq-hashtable)]
         [in-degree (make-eq-hashtable)]
         [graph (make-eq-hashtable)])
    ;; Initialize
    (for-each (lambda (m)
                (hashtable-set! mod-set m #t)
                (hashtable-set! in-degree m 0)
                (hashtable-set! graph m '()))
              modules)
    ;; Build graph edges within our module set
    (for-each
     (lambda (m)
       (let ([deps (hashtable-ref *module-deps* m '())])
         (for-each
          (lambda (dep)
            (when (hashtable-ref mod-set dep #f)
              ;; dep -> m edge (dep must load before m)
              (hashtable-set! graph dep (cons m (hashtable-ref graph dep '())))
              (hashtable-set! in-degree m (+ 1 (hashtable-ref in-degree m 0)))))
          deps)))
     modules)
    ;; Kahn's algorithm
    (let ([queue (filter (lambda (m) (zero? (hashtable-ref in-degree m 0))) modules)]
          [result '()])
      (let loop ([q queue] [res result])
        (if (null? q)
            (reverse res)
            (let* ([node (car q)]
                   [rest (cdr q)]
                   [neighbors (hashtable-ref graph node '())])
              (let ([new-queue
                     (fold-left
                      (lambda (acc neighbor)
                        (let ([new-deg (- (hashtable-ref in-degree neighbor 0) 1)])
                          (hashtable-set! in-degree neighbor new-deg)
                          (if (zero? new-deg)
                              (cons neighbor acc)
                              acc)))
                      rest
                      neighbors)])
                (loop new-queue (cons node res)))))))))

(doc reload! 'type (-> Symbol Void))
(doc reload! 'description "Reload a single module (re-execute its file). Preserves session state except for redefined bindings.")
(define (reload! name)
  (let ([path (module-name->path name)])
    (if path
        (let ([start (current-time-ms)])
          ;; Clear loaded status
          (hashtable-set! *module-registry* name (cons #f 0))
          ;; Re-load the file
          (load path)
          ;; Update registry with new load time
          (let ([duration (- (current-time-ms) start)])
            (hashtable-set! *module-registry* name (cons #t duration))
            (display (format "  Reloaded ~a (~ams)\n" name duration))))
        (display (format "  Error: Module ~a not found\n" name)))))

(doc reload-with-dependents! 'type (-> Symbol Void))
(doc reload-with-dependents! 'description "Reload a module and all its dependents in topological order.")
(define (reload-with-dependents! name)
  (build-dependents-index!)  ; Rebuild for accuracy
  (let* ([dependents (all-dependents name)]
         [to-reload (cons name dependents)]
         [sorted (topo-sort-reload to-reload)])
    (display (format "\n  Reloading ~a and ~a dependent(s):\n" name (length dependents)))
    (display (format "  Order: ~a\n\n" sorted))
    (for-each reload! sorted)
    (display "\n  Done.\n\n")))

;;; Convenience aliases
(doc rel! 'type (-> Symbol Void))
(doc rel! 'description "Alias for reload! (reload single module).")
(define rel! reload!)

(doc rel+! 'type (-> Symbol Void))
(doc rel+! 'description "Alias for reload-with-dependents! (reload module and dependents).")
(define rel+! reload-with-dependents!)
