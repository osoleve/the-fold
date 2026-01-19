;;; boundary/migrations/registry.ss — Migration Registry and Version Graph
;;;
;;; A registry for managing migrations and computing migration paths:
;;;
;;;   - Register migrations by name
;;;   - Build version graph from registered migrations
;;;   - Find shortest migration path between versions
;;;   - Chain migrations automatically
;;;
;;; The registry maintains a directed graph where:
;;;   - Nodes are versions (symbols like 'v1, 'v2)
;;;   - Edges are migrations (from-version -> to-version)
;;;
;;; This is Shell code: maintains mutable state.
;;;
;;; Dependencies:
;;;   - lattice/fp/optics/bidirectional.ss (for migration infrastructure)
;;;   - lattice/fp/optics/block-migration.ss (for block migrations)

(load "lattice/fp/optics/bidirectional.ss")
(load "lattice/fp/optics/block-migration.ss")

;;; ============================================================
;;; Part 1: Registry State
;;; ============================================================

;;; *migrations* : Hashtable Symbol -> Migration
;;; All registered migrations by name.
(define *migrations* (make-eq-hashtable))

;;; *version-graph* : Hashtable Version -> (List (Pair Version Migration))
;;; Adjacency list: version -> list of (target-version . migration)
(define *version-graph* (make-eq-hashtable))

;;; ============================================================
;;; Part 2: Registration
;;; ============================================================

;;; register-migration! : Migration -> Void
;;; Register a migration and update the version graph.
(define (register-migration! m)
  (let ([name (migration-name m)]
        [from (migration-from m)]
        [to (migration-to m)])
    ;; Store migration by name
    (hashtable-set! *migrations* name m)
    ;; Update version graph
    (let ([edges (hashtable-ref *version-graph* from '())])
      (hashtable-set! *version-graph* from
                      (cons (cons to m) edges)))
    ;; Also register the reverse for rollbacks
    (let ([flipped (migration-flip m)]
          [reverse-edges (hashtable-ref *version-graph* to '())])
      (hashtable-set! *version-graph* to
                      (cons (cons from flipped) reverse-edges)))))

;;; register-block-migration! : BlockMigration -> Void
;;; Register a block migration (converts to regular migration first).
(define (register-block-migration! bm)
  (register-migration! (block-migration->migration bm)))

;;; unregister-migration! : Symbol -> Boolean
;;; Remove a migration from the registry. Returns #t if found.
(define (unregister-migration! name)
  (let ([m (hashtable-ref *migrations* name #f)])
    (if (not m)
        #f
        (begin
          (hashtable-delete! *migrations* name)
          ;; Note: We don't update the graph - it would be expensive
          ;; and registries are typically append-only
          #t))))

;;; ============================================================
;;; Part 3: Lookup
;;; ============================================================

;;; get-migration : Symbol -> Migration | #f
;;; Look up a migration by name.
(define (get-migration name)
  (hashtable-ref *migrations* name #f))

;;; list-migrations : -> (List Migration)
;;; List all registered migrations.
(define (list-migrations)
  (let ([result '()])
    (vector-for-each
     (lambda (name)
       (set! result (cons (hashtable-ref *migrations* name #f) result)))
     (hashtable-keys *migrations*))
    result))

;;; list-versions : -> (List Version)
;;; List all known versions.
(define (list-versions)
  (let ([versions (make-eq-hashtable)])
    (vector-for-each
     (lambda (name)
       (let ([m (hashtable-ref *migrations* name #f)])
         (when m
           (hashtable-set! versions (migration-from m) #t)
           (hashtable-set! versions (migration-to m) #t))))
     (hashtable-keys *migrations*))
    (vector->list (hashtable-keys versions))))

;;; ============================================================
;;; Part 4: Path Finding (BFS)
;;; ============================================================
;;;
;;; Find shortest migration path between two versions.

;;; find-migration-path : Version -> Version -> (List Migration) | #f
;;; Find a path of migrations from source to target version.
;;; Returns #f if no path exists.
(define (find-migration-path source target)
  (if (equal? source target)
      '()  ; Already at target
      (let ([visited (make-eq-hashtable)]
            [queue (list (list source '()))])  ; (version . path-so-far)
        (hashtable-set! visited source #t)
        (let bfs ()
          (if (null? queue)
              #f  ; No path found
              (let* ([current (car queue)]
                     [version (car current)]
                     [path (cadr current)])
                (set! queue (cdr queue))
                ;; Check neighbors
                (let ([edges (hashtable-ref *version-graph* version '())])
                  (let check-edges ([edges edges])
                    (if (null? edges)
                        (bfs)  ; Continue BFS
                        (let* ([edge (car edges)]
                               [next-version (car edge)]
                               [migration (cdr edge)])
                          (if (equal? next-version target)
                              ;; Found target - return path
                              (reverse (cons migration path))
                              ;; Not target - continue if not visited
                              (begin
                                (unless (hashtable-ref visited next-version #f)
                                  (hashtable-set! visited next-version #t)
                                  (set! queue
                                        (append queue
                                                (list (list next-version
                                                           (cons migration path))))))
                                (check-edges (cdr edges))))))))))))))

;;; ============================================================
;;; Part 5: Automatic Migration Chain
;;; ============================================================

;;; get-migration-chain : Version -> Version -> Migration | #f
;;; Get a composed migration from source to target.
;;; Automatically finds and chains required migrations.
(define (get-migration-chain source target)
  (let ([path (find-migration-path source target)])
    (if (not path)
        #f
        (if (null? path)
            (make-identity-migration 'identity source)
            (migration-chain path)))))

;;; ============================================================
;;; Part 6: Version Compatibility
;;; ============================================================

;;; versions-connected? : Version -> Version -> Boolean
;;; Check if there's any path between two versions.
(define (versions-connected? v1 v2)
  (not (not (find-migration-path v1 v2))))

;;; latest-version : Symbol -> Version | #f
;;; Find the latest version for a type (highest version number).
;;; Assumes versioned tags like 'type-v1, 'type-v2.
(define (latest-version type-prefix)
  (let ([versions (filter (lambda (v)
                            (let ([parsed (parse-versioned-tag v)])
                              (and (eq? (car parsed) type-prefix)
                                   (cdr parsed))))
                          (list-versions))])
    (if (null? versions)
        #f
        (let ([sorted (sort (lambda (a b)
                              (> (cdr (parse-versioned-tag a))
                                 (cdr (parse-versioned-tag b))))
                            versions)])
          (car sorted)))))

;;; ============================================================
;;; Part 7: Registry Persistence
;;; ============================================================

;;; save-registry! : String -> Void
;;; Save registry to a file as S-expression.
(define (save-registry! path)
  (call-with-output-file path
    (lambda (port)
      (display ";; Migration Registry\n" port)
      (display ";; Auto-generated, do not edit\n\n" port)
      (for-each
       (lambda (m)
         (let ([name (migration-name m)]
               [from (migration-from m)]
               [to (migration-to m)])
           (display (format "(migration ~a ~a ~a)\n" name from to) port)))
       (list-migrations)))))

;;; ============================================================
;;; Part 8: Predefined Migrations
;;; ============================================================
;;;
;;; Common migrations for The Fold's types.

;;; define-bbs-migrations! : -> Void
;;; Register standard BBS issue migrations.
(define (define-bbs-migrations!)
  ;; v1 -> v2: Add created-at timestamp
  (register-block-migration!
   (make-block-migration
    'bbs-issue-v1 'bbs-issue-v2
    (field-add-iso 'created-at 0)))

  ;; v2 -> v3: Rename 'desc' to 'description'
  (register-block-migration!
   (make-block-migration
    'bbs-issue-v2 'bbs-issue-v3
    (field-rename-iso 'desc 'description))))

;;; ============================================================
;;; Part 9: Registry Inspection
;;; ============================================================

;;; describe-registry : -> String
;;; Generate a human-readable summary of the registry.
(define (describe-registry)
  (let ([migrations (list-migrations)]
        [versions (list-versions)])
    (format "Migration Registry:
  Migrations: ~a
  Versions: ~a
  Edges: ~a"
            (length migrations)
            (length versions)
            (fold-left + 0
                       (map (lambda (v)
                              (length (hashtable-ref *version-graph* v '())))
                            versions)))))

;;; version-graph->dot : -> String
;;; Generate GraphViz DOT representation of version graph.
(define (version-graph->dot)
  (let ([lines '("digraph migrations {")]
        [versions (list-versions)])
    (for-each
     (lambda (v)
       (let ([edges (hashtable-ref *version-graph* v '())])
         (for-each
          (lambda (edge)
            (let ([target (car edge)]
                  [m (cdr edge)])
              (set! lines
                    (cons (format "  \"~a\" -> \"~a\" [label=\"~a\"];"
                                  v target (migration-name m))
                          lines))))
          ;; Only show forward edges (not flipped)
          (filter (lambda (edge)
                    (not (string-suffix?
                          "-reversed"
                          (symbol->string (migration-name (cdr edge))))))
                  edges))))
     versions)
    (set! lines (cons "}" lines))
    (apply string-append
           (map (lambda (l) (string-append l "\n"))
                (reverse lines)))))

;;; string-suffix? : String -> String -> Boolean
(define (string-suffix? suffix str)
  (let ([slen (string-length suffix)]
        [len (string-length str)])
    (and (>= len slen)
         (string=? suffix (substring str (- len slen) len)))))

;;; ============================================================
;;; Part 10: Clear Registry
;;; ============================================================

;;; clear-registry! : -> Void
;;; Remove all registered migrations.
(define (clear-registry!)
  (set! *migrations* (make-eq-hashtable))
  (set! *version-graph* (make-eq-hashtable)))

;;; ============================================================
;;; Exports
;;; ============================================================
;;;
;;; Registration:
;;;   register-migration!, register-block-migration!, unregister-migration!
;;;
;;; Lookup:
;;;   get-migration, list-migrations, list-versions
;;;
;;; Path Finding:
;;;   find-migration-path, get-migration-chain
;;;
;;; Compatibility:
;;;   versions-connected?, latest-version
;;;
;;; Persistence:
;;;   save-registry!
;;;
;;; Predefined:
;;;   define-bbs-migrations!
;;;
;;; Inspection:
;;;   describe-registry, version-graph->dot
;;;
;;; Utilities:
;;;   clear-registry!

(display "Loaded: boundary/migrations/registry.ss\n")
