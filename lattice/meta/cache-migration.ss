(load "lattice/optics/bidirectional.ss")
(load "lattice/optics/schema.ss")

(doc 'module 'cache-migration)
(doc 'description "Bidirectional cache format migrations for smooth upgrades")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================
;;; Part 1: Cache Migration Registry
;;; ============================================================

;;; Cache migrations indexed by (from-version . to-version)
;;; Use equal-hash for cons cell keys
(define *cache-migrations* (make-hashtable equal-hash equal?))

;;; register-cache-migration! : Int -> Int -> PIso -> Void
;;; Register a migration between cache versions.
(define (register-cache-migration! from-version to-version iso)
  (hashtable-set! *cache-migrations*
                  (cons from-version to-version)
                  (make-migration
                   (string->symbol (format "cache-v~a->v~a" from-version to-version))
                   from-version to-version iso)))

;;; get-cache-migration : Int -> Int -> Migration | #f
(define (get-cache-migration from to)
  (hashtable-ref *cache-migrations* (cons from to) #f))

;;; ============================================================
;;; Part 2: Cache Version Detection
;;; ============================================================

;;; cache-version : SExp -> Int
;;; Extract version from cache S-expression.
(define (cache-version cache)
  (if (and (pair? cache) (eq? (car cache) 'lattice-cache))
      (let ([version-pair (assq 'version (cdr cache))])
        (if version-pair
            (cadr version-pair)
            0))
      0))

;;; cache-set-version : SExp -> Int -> SExp
;;; Update version in cache S-expression.
(define (cache-set-version cache new-version)
  (if (and (pair? cache) (eq? (car cache) 'lattice-cache))
      (cons 'lattice-cache
            (cons (list 'version new-version)
                  (filter (lambda (p) (not (and (pair? p) (eq? (car p) 'version))))
                          (cdr cache))))
      cache))

;;; ============================================================
;;; Part 3: Migration Path Finding
;;; ============================================================

;;; find-migration-path : Int -> Int -> (List Migration) | #f
;;; Find a sequence of migrations from source to target version.
;;; Uses BFS for shortest path.
(define (find-cache-migration-path from-version to-version)
  (if (= from-version to-version)
      '()  ; Already at target
      (let ([visited (make-eqv-hashtable)]
            [queue (list (list from-version '()))])
        (hashtable-set! visited from-version #t)
        (let bfs ()
          (if (null? queue)
              #f  ; No path
              (let* ([current (car queue)]
                     [version (car current)]
                     [path (cadr current)])
                (set! queue (cdr queue))
                ;; Try all registered migrations from this version
                (let check-migrations ([keys (vector->list (hashtable-keys *cache-migrations*))])
                  (if (null? keys)
                      (bfs)  ; Continue BFS
                      (let* ([key (car keys)]
                             [from (car key)]
                             [to (cdr key)])
                        (if (not (= from version))
                            (check-migrations (cdr keys))
                            (let ([migration (hashtable-ref *cache-migrations* key #f)])
                              (if (= to to-version)
                                  ;; Found target
                                  (reverse (cons migration path))
                                  ;; Not target, continue if not visited
                                  (begin
                                    (unless (hashtable-ref visited to #f)
                                      (hashtable-set! visited to #t)
                                      (set! queue
                                            (append queue
                                                    (list (list to (cons migration path))))))
                                    (check-migrations (cdr keys)))))))))))))))

;;; ============================================================
;;; Part 4: Apply Migrations
;;; ============================================================

;;; migrate-cache : SExp -> Int -> SExp
;;; Migrate cache data from its current version to target version.
;;; Returns migrated cache or original if no path exists.
(define (migrate-cache cache target-version)
  (let* ([current-version (cache-version cache)]
         [path (find-cache-migration-path current-version target-version)])
    (if (not path)
        cache  ; No migration path, return unchanged
        (let migrate-step ([cache cache] [migrations path])
          (if (null? migrations)
              (cache-set-version cache target-version)
              (let* ([m (car migrations)]
                     [migrated (migrate m cache)])
                (migrate-step migrated (cdr migrations))))))))

;;; rollback-cache : SExp -> Int -> SExp
;;; Rollback cache to an earlier version.
(define (rollback-cache cache target-version)
  (let* ([current-version (cache-version cache)]
         ;; Find path from target to current, then flip each migration
         [path (find-cache-migration-path target-version current-version)])
    (if (not path)
        cache
        (let rollback-step ([cache cache] [migrations (reverse path)])
          (if (null? migrations)
              (cache-set-version cache target-version)
              (let* ([m (car migrations)]
                     [rolled-back (rollback m cache)])
                (rollback-step rolled-back (cdr migrations))))))))

;;; ============================================================
;;; Part 5: Predefined Cache Migrations
;;; ============================================================
;;;
;;; Define migrations between cache format versions here.
;;; Each migration should handle the schema changes between versions.

;;; v1 -> v2: Add 'exports-count' field to kg-state
;;; (This is an example - real migrations would be added as needed)
(define cache-v1->v2-iso
  (make-p-iso
   ;; Forward: Add exports-count to kg-state
   (lambda (cache)
     (let* ([kg-state-pair (assq 'kg-state (cdr cache))]
            [kg-state (if kg-state-pair (cdr kg-state-pair) '())])
       ;; If kg-state has export-names, compute count
       ;; Note: format is (export-names (x y z)), so cadr to get the list
       (let* ([export-pair (assq 'export-names kg-state)]
              [export-names (if export-pair (cadr export-pair) '())]
              [count (if (list? export-names) (length export-names) 0)])
         (if kg-state-pair
             (cons (car cache)
                   (map (lambda (field)
                          (if (and (pair? field) (eq? (car field) 'kg-state))
                              (cons 'kg-state
                                    (cons (list 'exports-count count)
                                          (cdr field)))
                              field))
                        (cdr cache)))
             cache))))
   ;; Backward: Remove exports-count from kg-state
   (lambda (cache)
     (cons (car cache)
           (map (lambda (field)
                  (if (and (pair? field) (eq? (car field) 'kg-state))
                      (cons 'kg-state
                            (filter (lambda (f)
                                      (not (and (pair? f) (eq? (car f) 'exports-count))))
                                    (cdr field)))
                      field))
                (cdr cache))))))

;; Register the example migration
;; (register-cache-migration! 1 2 cache-v1->v2-iso)

;;; ============================================================
;;; Part 6: Integration with persist.ss
;;; ============================================================

;;; try-migrate-cache : SExp -> Int -> SExp
;;; Attempt to migrate cache to target version.
;;; Returns migrated cache on success, #f if incompatible.
(define (try-migrate-cache cache target-version)
  (let ([current (cache-version cache)])
    (cond
      [(= current target-version) cache]  ; Already correct version
      [(find-cache-migration-path current target-version)
       => (lambda (path)
            (printf "Migrating cache from v~a to v~a (~a steps)...\n"
                    current target-version (length path))
            (migrate-cache cache target-version))]
      [else #f])))  ; No migration path

;;; cache-migrations-available : Int -> Int -> Bool
;;; Check if a migration path exists between versions.
(define (cache-migrations-available from to)
  (and (find-cache-migration-path from to) #t))

;;; list-cache-migrations : -> (List (Int . Int))
;;; List all registered cache migrations.
(define (list-cache-migrations)
  (vector->list (hashtable-keys *cache-migrations*)))

;;; ============================================================
;;; Exports
;;; ============================================================
;;;
;;; Registration:
;;;   register-cache-migration!, get-cache-migration
;;;
;;; Version handling:
;;;   cache-version, cache-set-version
;;;
;;; Path finding:
;;;   find-cache-migration-path, cache-migrations-available
;;;
;;; Migration:
;;;   migrate-cache, rollback-cache, try-migrate-cache
;;;
;;; Introspection:
;;;   list-cache-migrations

