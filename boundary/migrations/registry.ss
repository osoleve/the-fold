(load "lattice/optics/bidirectional.ss")
(load "lattice/optics/block-migration.ss")

(doc 'module 'migrations/registry)
(doc 'description "Migration registry and version graph for managing schema evolution")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(lattice/optics/bidirectional lattice/optics/block-migration))

(doc 'note "A registry for managing migrations and computing migration paths: register migrations by name, build version graph, find shortest migration path, chain migrations automatically")
(doc 'note "The registry maintains a directed graph where nodes are versions (symbols like v1, v2) and edges are migrations (from-version -> to-version)")

(doc 'section 'registry-state)

(doc *migrations* 'type (Hashtable Symbol Migration))
(doc *migrations* 'description "All registered migrations by name")
(define *migrations* (make-eq-hashtable))

(doc *version-graph* 'type (Hashtable Version (List (Pair Version Migration))))
(doc *version-graph* 'description "Adjacency list: version -> list of (target-version . migration)")
(define *version-graph* (make-eq-hashtable))

(doc 'section 'registration)

(define (register-migration! m)
  (doc 'type (-> Migration Void))
  (doc 'description "Register a migration and update the version graph")
  (doc 'export #t)
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

(define (register-block-migration! bm)
  (doc 'type (-> BlockMigration Void))
  (doc 'description "Register a block migration (converts to regular migration first)")
  (doc 'export #t)
  (register-migration! (block-migration->migration bm)))

(define (unregister-migration! name)
  (doc 'type (-> Symbol Boolean))
  (doc 'description "Remove a migration from the registry, returns #t if found")
  (doc 'export #t)
  (doc 'note "Does not update the graph (expensive and registries are typically append-only)")
  (let ([m (hashtable-ref *migrations* name #f)])
    (if (not m)
        #f
        (begin
          (hashtable-delete! *migrations* name)
          #t))))

(doc 'section 'lookup)

(define (get-migration name)
  (doc 'type (-> Symbol (Maybe Migration)))
  (doc 'description "Look up a migration by name")
  (doc 'export #t)
  (hashtable-ref *migrations* name #f))

(define (list-migrations)
  (doc 'type (-> (List Migration)))
  (doc 'description "List all registered migrations")
  (doc 'export #t)
  (let ([result '()])
    (vector-for-each
     (lambda (name)
       (set! result (cons (hashtable-ref *migrations* name #f) result)))
     (hashtable-keys *migrations*))
    result))

(define (list-versions)
  (doc 'type (-> (List Version)))
  (doc 'description "List all known versions")
  (doc 'export #t)
  (let ([versions (make-eq-hashtable)])
    (vector-for-each
     (lambda (name)
       (let ([m (hashtable-ref *migrations* name #f)])
         (when m
           (hashtable-set! versions (migration-from m) #t)
           (hashtable-set! versions (migration-to m) #t))))
     (hashtable-keys *migrations*))
    (vector->list (hashtable-keys versions))))

(doc 'section 'path-finding)
(doc 'note "Find shortest migration path between two versions using BFS")

(define (find-migration-path source target)
  (doc 'type (-> Version Version (Maybe (List Migration))))
  (doc 'description "Find a path of migrations from source to target version, returns #f if no path exists")
  (doc 'export #t)
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

(doc 'section 'automatic-migration-chain)

(define (get-migration-chain source target)
  (doc 'type (-> Version Version (Maybe Migration)))
  (doc 'description "Get a composed migration from source to target, automatically finds and chains required migrations")
  (doc 'export #t)
  (let ([path (find-migration-path source target)])
    (if (not path)
        #f
        (if (null? path)
            (make-identity-migration 'identity source)
            (migration-chain path)))))

(doc 'section 'version-compatibility)

(define (versions-connected? v1 v2)
  (doc 'type (-> Version Version Boolean))
  (doc 'description "Check if there's any path between two versions")
  (doc 'export #t)
  (not (not (find-migration-path v1 v2))))

(define (latest-version type-prefix)
  (doc 'type (-> Symbol (Maybe Version)))
  (doc 'description "Find the latest version for a type (highest version number), assumes versioned tags like type-v1, type-v2")
  (doc 'export #t)
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

(doc 'section 'registry-persistence)

(define (save-registry! path)
  (doc 'type (-> String Void))
  (doc 'description "Save registry to a file as S-expression")
  (doc 'export #t)
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

(doc 'section 'predefined-migrations)
(doc 'note "Common migrations for The Fold's types")

(define (define-bbs-migrations!)
  (doc 'type (-> Void))
  (doc 'description "Register standard BBS issue migrations")
  (doc 'export #t)
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

(doc 'section 'registry-inspection)

(define (describe-registry)
  (doc 'type (-> String))
  (doc 'description "Generate a human-readable summary of the registry")
  (doc 'export #t)
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

(define (version-graph->dot)
  (doc 'type (-> String))
  (doc 'description "Generate GraphViz DOT representation of version graph")
  (doc 'export #t)
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

(define (string-suffix? suffix str)
  (doc 'type (-> String String Boolean))
  (doc 'description "Check if string ends with suffix")
  (let ([slen (string-length suffix)]
        [len (string-length str)])
    (and (>= len slen)
         (string=? suffix (substring str (- len slen) len)))))

(doc 'section 'clear-registry)

(define (clear-registry!)
  (doc 'type (-> Void))
  (doc 'description "Remove all registered migrations")
  (doc 'export #t)
  (set! *migrations* (make-eq-hashtable))
  (set! *version-graph* (make-eq-hashtable)))

(doc 'section 'exports)
(doc 'note "Registration: register-migration!, register-block-migration!, unregister-migration!")
(doc 'note "Lookup: get-migration, list-migrations, list-versions")
(doc 'note "Path Finding: find-migration-path, get-migration-chain")
(doc 'note "Compatibility: versions-connected?, latest-version")
(doc 'note "Persistence: save-registry!")
(doc 'note "Predefined: define-bbs-migrations!")
(doc 'note "Inspection: describe-registry, version-graph->dot")
(doc 'note "Utilities: clear-registry!")

(display "Loaded: boundary/migrations/registry.ss\n")
