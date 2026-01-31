;;; lattice/physics/classical/constraint-graph.ss — Graph-based constraint analysis
;;; @module constraint-graph
;;; @requires prelude constraints

(load "core/base/prelude.ss")

(doc 'module 'constraint-graph)
(doc 'description "Graph algorithms for physics constraint systems: islands, ordering, parallelization")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'section 'overview)

(doc 'note "KEY CONCEPTS:
  - Constraint Graph: Entities are nodes, constraints are edges
  - Islands: Connected components (can be solved independently)
  - Rigid Clusters: Strongly connected components (tightly coupled)
  - Graph Coloring: Parallel solving within islands")

;;; ============================================================
;;; Section 1: Graph Representation
;;; ============================================================

(doc 'section 'representation)

(doc 'make-constraint-graph 'type '(-> (List Constraint) ConstraintGraph))
(doc 'make-constraint-graph 'description "Build graph from constraints; entities=nodes, constraints=edges")
(define (make-constraint-graph constraints)
  ;; Build adjacency list and constraint lookup
  (let ([adj (make-hashtable equal-hash equal?)]       ; entity-id -> list of neighbor ids
        [entity-constraints (make-hashtable equal-hash equal?)]  ; entity-id -> list of constraints
        [all-entities '()])
    ;; Process each constraint
    (for-each
     (lambda (c)
       (let ([entity-a (constraint-entity-a c)]
             [entity-b (constraint-entity-b c)])
         ;; Add entity-a
         (unless (hashtable-contains? adj entity-a)
           (hashtable-set! adj entity-a '())
           (hashtable-set! entity-constraints entity-a '())
           (set! all-entities (cons entity-a all-entities)))
         ;; Add to a's constraint list
         (hashtable-set! entity-constraints entity-a
                         (cons c (hashtable-ref entity-constraints entity-a '())))
         ;; If entity-b exists (not anchored to world)
         (when entity-b
           ;; Add entity-b
           (unless (hashtable-contains? adj entity-b)
             (hashtable-set! adj entity-b '())
             (hashtable-set! entity-constraints entity-b '())
             (set! all-entities (cons entity-b all-entities)))
           ;; Add to b's constraint list
           (hashtable-set! entity-constraints entity-b
                           (cons c (hashtable-ref entity-constraints entity-b '())))
           ;; Add edges (undirected: a->b and b->a)
           (hashtable-set! adj entity-a
                           (cons entity-b (hashtable-ref adj entity-a '())))
           (hashtable-set! adj entity-b
                           (cons entity-a (hashtable-ref adj entity-b '()))))))
     constraints)
    ;; Return graph structure
    (list 'constraint-graph adj entity-constraints (reverse all-entities) constraints)))

(doc 'constraint-graph? 'type '(-> Any Boolean))
(define (constraint-graph? g)
  (and (pair? g) (eq? (car g) 'constraint-graph)))

(define (cg-adjacency g) (list-ref g 1))
(define (cg-entity-constraints g) (list-ref g 2))
(define (cg-entities g) (list-ref g 3))
(define (cg-constraints g) (list-ref g 4))

(doc 'cg-neighbors 'type '(-> ConstraintGraph EntityId (List EntityId)))
(doc 'cg-neighbors 'description "Get neighbor entities of a given entity")
(define (cg-neighbors graph entity-id)
  (hashtable-ref (cg-adjacency graph) entity-id '()))

(doc 'cg-constraints-for 'type '(-> ConstraintGraph EntityId (List Constraint)))
(doc 'cg-constraints-for 'description "Get constraints involving a given entity")
(define (cg-constraints-for graph entity-id)
  (hashtable-ref (cg-entity-constraints graph) entity-id '()))

;;; ============================================================
;;; Section 2: Island Detection (Connected Components)
;;; ============================================================

(doc 'section 'islands)
(doc 'description "Islands are connected components in the constraint graph.
Each island can be solved independently, enabling parallel simulation.")

(doc 'cg-find-islands 'type '(-> ConstraintGraph (List Island)))
(doc 'cg-find-islands 'description "Find all constraint islands (connected components)")
(define (cg-find-islands graph)
  (let ([entities (cg-entities graph)]
        [visited (make-hashtable equal-hash equal?)]
        [islands '()])
    ;; BFS from each unvisited entity
    (for-each
     (lambda (start)
       (unless (hashtable-ref visited start #f)
         (let ([island-entities '()]
               [island-constraints '()]
               [constraint-set (make-hashtable equal-hash equal?)])
           ;; BFS
           (let bfs ([queue (list start)])
             (unless (null? queue)
               (let ([current (car queue)]
                     [rest (cdr queue)])
                 (if (hashtable-ref visited current #f)
                     (bfs rest)
                     (begin
                       (hashtable-set! visited current #t)
                       (set! island-entities (cons current island-entities))
                       ;; Collect constraints (avoid duplicates)
                       (for-each
                        (lambda (c)
                          (unless (hashtable-ref constraint-set (constraint-id c) #f)
                            (hashtable-set! constraint-set (constraint-id c) #t)
                            (set! island-constraints (cons c island-constraints))))
                        (cg-constraints-for graph current))
                       ;; Add unvisited neighbors to queue
                       (let ([neighbors (filter
                                          (lambda (n) (not (hashtable-ref visited n #f)))
                                          (cg-neighbors graph current))])
                         (bfs (append rest neighbors))))))))
           ;; Record island
           (set! islands (cons (make-island (reverse island-entities)
                                            (reverse island-constraints))
                               islands)))))
     entities)
    (reverse islands)))

(doc 'make-island 'type '(-> (List EntityId) (List Constraint) Island))
(define (make-island entities constraints)
  (list 'island entities constraints))

(doc 'island? 'type '(-> Any Boolean))
(define (island? x)
  (and (pair? x) (eq? (car x) 'island)))

(define (island-entities island) (list-ref island 1))
(define (island-constraints island) (list-ref island 2))

(doc 'island-size 'type '(-> Island Nat))
(doc 'island-size 'description "Number of entities in island")
(define (island-size island)
  (length (island-entities island)))

(doc 'island-constraint-count 'type '(-> Island Nat))
(doc 'island-constraint-count 'description "Number of constraints in island")
(define (island-constraint-count island)
  (length (island-constraints island)))

;;; ============================================================
;;; Section 3: Strongly Connected Components (Tarjan's Algorithm)
;;; ============================================================

(doc 'section 'sccs)
(doc 'description "Strongly connected components identify rigid clusters:
groups of bodies so tightly coupled they move as units.
Uses Tarjan's algorithm for O(V+E) complexity.")

(doc 'cg-find-sccs 'type '(-> ConstraintGraph (List SCC)))
(doc 'cg-find-sccs 'description "Find strongly connected components using Tarjan's algorithm")
(define (cg-find-sccs graph)
  (let ([entities (cg-entities graph)]
        [index-counter 0]
        [indices (make-hashtable equal-hash equal?)]    ; entity -> index
        [lowlinks (make-hashtable equal-hash equal?)]   ; entity -> lowlink
        [on-stack (make-hashtable equal-hash equal?)]   ; entity -> boolean
        [stack '()]
        [sccs '()])

    (define (strongconnect entity)
      ;; Set index and lowlink
      (hashtable-set! indices entity index-counter)
      (hashtable-set! lowlinks entity index-counter)
      (set! index-counter (+ index-counter 1))
      (set! stack (cons entity stack))
      (hashtable-set! on-stack entity #t)

      ;; Consider successors
      (for-each
       (lambda (neighbor)
         (cond
          ;; Successor not yet visited
          [(not (hashtable-contains? indices neighbor))
           (strongconnect neighbor)
           (hashtable-set! lowlinks entity
                           (min (hashtable-ref lowlinks entity 0)
                                (hashtable-ref lowlinks neighbor 0)))]
          ;; Successor on stack -> in current SCC
          [(hashtable-ref on-stack neighbor #f)
           (hashtable-set! lowlinks entity
                           (min (hashtable-ref lowlinks entity 0)
                                (hashtable-ref indices neighbor 0)))]))
       (cg-neighbors graph entity))

      ;; If entity is root of SCC, pop stack and generate SCC
      (when (= (hashtable-ref lowlinks entity 0)
               (hashtable-ref indices entity 0))
        (let loop ([scc-entities '()])
          (let ([w (car stack)])
            (set! stack (cdr stack))
            (hashtable-set! on-stack w #f)
            (if (equal? w entity)
                ;; Complete SCC
                (set! sccs (cons (cons entity scc-entities) sccs))
                ;; Continue popping
                (loop (cons w scc-entities)))))))

    ;; Process all entities
    (for-each
     (lambda (entity)
       (unless (hashtable-contains? indices entity)
         (strongconnect entity)))
     entities)

    ;; Convert to SCC structures
    (map (lambda (entity-list)
           (make-scc entity-list (scc-internal-constraints graph entity-list)))
         (reverse sccs))))

(doc 'make-scc 'type '(-> (List EntityId) (List Constraint) SCC))
(define (make-scc entities constraints)
  (list 'scc entities constraints))

(doc 'scc? 'type '(-> Any Boolean))
(define (scc? x)
  (and (pair? x) (eq? (car x) 'scc)))

(define (scc-entities scc) (list-ref scc 1))
(define (scc-constraints scc) (list-ref scc 2))

(doc 'scc-internal-constraints 'type '(-> ConstraintGraph (List EntityId) (List Constraint)))
(doc 'scc-internal-constraints 'description "Get constraints where both entities are in the SCC")
(define (scc-internal-constraints graph entity-list)
  (let ([entity-set (make-hashtable equal-hash equal?)]
        [seen (make-hashtable equal-hash equal?)]
        [result '()])
    ;; Build set for O(1) membership
    (for-each (lambda (e) (hashtable-set! entity-set e #t)) entity-list)
    ;; Find internal constraints
    (for-each
     (lambda (entity)
       (for-each
        (lambda (c)
          (unless (hashtable-ref seen (constraint-id c) #f)
            (hashtable-set! seen (constraint-id c) #t)
            (let ([a (constraint-entity-a c)]
                  [b (constraint-entity-b c)])
              ;; Internal if both endpoints in SCC (or b is #f for world anchor)
              (when (and (hashtable-ref entity-set a #f)
                         (or (not b) (hashtable-ref entity-set b #f)))
                (set! result (cons c result))))))
        (cg-constraints-for graph entity)))
     entity-list)
    (reverse result)))

(doc 'scc-size 'type '(-> SCC Nat))
(define (scc-size scc)
  (length (scc-entities scc)))

(doc 'cg-scc-dag 'type '(-> ConstraintGraph (List SCC) SCCGraph))
(doc 'cg-scc-dag 'description "Build DAG of SCCs for ordering constraint groups")
(define (cg-scc-dag graph sccs)
  ;; Map entity to its SCC index
  (let ([entity->scc (make-hashtable equal-hash equal?)]
        [n (length sccs)])
    ;; Build entity->scc mapping
    (let loop ([ss sccs] [idx 0])
      (unless (null? ss)
        (for-each
         (lambda (entity)
           (hashtable-set! entity->scc entity idx))
         (scc-entities (car ss)))
        (loop (cdr ss) (+ idx 1))))
    ;; Build adjacency for SCC DAG
    (let ([adj (make-vector n '())])
      (for-each
       (lambda (c)
         (let* ([a (constraint-entity-a c)]
                [b (constraint-entity-b c)]
                [scc-a (hashtable-ref entity->scc a #f)]
                [scc-b (and b (hashtable-ref entity->scc b #f))])
           ;; Edge between different SCCs
           (when (and scc-a scc-b (not (= scc-a scc-b)))
             (unless (member scc-b (vector-ref adj scc-a))
               (vector-set! adj scc-a (cons scc-b (vector-ref adj scc-a)))))))
       (cg-constraints graph))
      (list 'scc-graph sccs adj))))

;;; ============================================================
;;; Section 4: Topological Ordering
;;; ============================================================

(doc 'section 'toposort)
(doc 'description "Topological sort orders constraints for sequential solving,
ensuring dependencies are resolved before dependents.")

(doc 'cg-toposort-constraints 'type '(-> ConstraintGraph (Maybe (List Constraint))))
(doc 'cg-toposort-constraints 'description "Topologically sort constraints; returns #f if cyclic")
(define (cg-toposort-constraints graph)
  ;; Build constraint dependency graph based on shared entities
  ;; A constraint C1 "depends on" C2 if they share an entity and C2 comes first
  ;; For simplicity, use SCC-based ordering
  (let* ([sccs (cg-find-sccs graph)]
         [scc-dag (cg-scc-dag graph sccs)]
         [sorted-scc-indices (toposort-dag (list-ref scc-dag 2))])
    (if sorted-scc-indices
        ;; Flatten constraints in topological order
        (let ([sccs-vec (list->vector sccs)])
          (fold-right
           (lambda (idx acc)
             (append (scc-constraints (vector-ref sccs-vec idx)) acc))
           '()
           sorted-scc-indices))
        #f)))

(doc 'toposort-dag 'type '(-> (Vector (List Nat)) (Maybe (List Nat))))
(doc 'toposort-dag 'description "Topological sort on adjacency vector; returns #f if cyclic")
(define (toposort-dag adj)
  (let* ([n (vector-length adj)]
         [in-degree (make-vector n 0)]
         [result '()])
    ;; Calculate in-degrees
    (let loop ([i 0])
      (when (< i n)
        (for-each
         (lambda (j)
           (vector-set! in-degree j (+ 1 (vector-ref in-degree j))))
         (vector-ref adj i))
        (loop (+ i 1))))
    ;; Find initial zero in-degree nodes
    (let ([queue (let loop ([i 0] [q '()])
                   (if (>= i n)
                       q
                       (if (= 0 (vector-ref in-degree i))
                           (loop (+ i 1) (cons i q))
                           (loop (+ i 1) q))))])
      ;; Kahn's algorithm
      (let process ([q queue])
        (unless (null? q)
          (let ([node (car q)]
                [rest (cdr q)])
            (set! result (cons node result))
            ;; Decrease in-degree of neighbors
            (let ([new-queue
                   (fold-left
                    (lambda (q neighbor)
                      (let ([new-deg (- (vector-ref in-degree neighbor) 1)])
                        (vector-set! in-degree neighbor new-deg)
                        (if (= new-deg 0)
                            (cons neighbor q)
                            q)))
                    rest
                    (vector-ref adj node))])
              (process new-queue)))))
      ;; Check if all nodes processed
      (if (= (length result) n)
          (reverse result)
          #f))))

;;; ============================================================
;;; Section 5: Graph Coloring for Parallel Solving
;;; ============================================================

(doc 'section 'coloring)
(doc 'description "Graph coloring assigns colors to constraints such that
no two constraints sharing an entity have the same color.
Constraints with the same color can be solved in parallel.")

(doc 'cg-color-constraints 'type '(-> ConstraintGraph (List (List Constraint))))
(doc 'cg-color-constraints 'description "Color constraints for parallelization; returns list of parallel groups")
(define (cg-color-constraints graph)
  (let* ([constraints (cg-constraints graph)]
         [n (length constraints)]
         [constraint-vec (list->vector constraints)]
         [colors (make-vector n -1)]  ; -1 = uncolored
         ;; Build constraint adjacency (conflicts)
         [conflict-adj (build-constraint-conflicts graph constraints)])
    ;; Greedy coloring
    (let loop ([i 0])
      (when (< i n)
        ;; Find used colors among neighbors
        (let ([used (make-eqv-hashtable)])
          (for-each
           (lambda (neighbor-idx)
             (let ([c (vector-ref colors neighbor-idx)])
               (when (>= c 0)
                 (hashtable-set! used c #t))))
           (vector-ref conflict-adj i))
          ;; Find smallest unused color
          (let find-color ([c 0])
            (if (hashtable-ref used c #f)
                (find-color (+ c 1))
                (vector-set! colors i c))))
        (loop (+ i 1))))
    ;; Group constraints by color
    (let ([groups (make-eqv-hashtable)])
      (let loop ([i 0])
        (when (< i n)
          (let ([c (vector-ref colors i)])
            (hashtable-set! groups c
                            (cons (vector-ref constraint-vec i)
                                  (hashtable-ref groups c '()))))
          (loop (+ i 1))))
      ;; Return as sorted list of groups
      (let-values ([(keys vals) (hashtable-entries groups)])
        (map reverse (vector->list vals))))))

(doc 'build-constraint-conflicts 'type '(-> ConstraintGraph (List Constraint) (Vector (List Nat))))
(doc 'build-constraint-conflicts 'description "Build adjacency: constraints conflict if they share an entity")
(define (build-constraint-conflicts graph constraints)
  (let* ([n (length constraints)]
         [adj (make-vector n '())]
         ;; Map constraint-id to index
         [id->idx (make-hashtable equal-hash equal?)])
    ;; Build id->idx mapping
    (let loop ([cs constraints] [i 0])
      (unless (null? cs)
        (hashtable-set! id->idx (constraint-id (car cs)) i)
        (loop (cdr cs) (+ i 1))))
    ;; For each entity, mark constraints as conflicting
    (for-each
     (lambda (entity)
       (let ([entity-constraints (cg-constraints-for graph entity)])
         ;; All pairs conflict
         (for-each
          (lambda (c1)
            (let ([i (hashtable-ref id->idx (constraint-id c1) #f)])
              (when i
                (for-each
                 (lambda (c2)
                   (let ([j (hashtable-ref id->idx (constraint-id c2) #f)])
                     (when (and j (not (= i j)))
                       (unless (member j (vector-ref adj i))
                         (vector-set! adj i (cons j (vector-ref adj i)))))))
                 entity-constraints))))
          entity-constraints)))
     (cg-entities graph))
    adj))

(doc 'cg-chromatic-number 'type '(-> ConstraintGraph Nat))
(doc 'cg-chromatic-number 'description "Number of colors used (= number of parallel groups)")
(define (cg-chromatic-number graph)
  (length (cg-color-constraints graph)))

;;; ============================================================
;;; Section 6: Utility Functions
;;; ============================================================

(doc 'section 'utilities)

(doc 'cg-stats 'type '(-> ConstraintGraph Alist))
(doc 'cg-stats 'description "Compute statistics about constraint graph")
(define (cg-stats graph)
  (let* ([entities (cg-entities graph)]
         [constraints (cg-constraints graph)]
         [islands (cg-find-islands graph)]
         [sccs (cg-find-sccs graph)]
         [colors (cg-color-constraints graph)])
    `((entities . ,(length entities))
      (constraints . ,(length constraints))
      (islands . ,(length islands))
      (largest-island . ,(if (null? islands)
                             0
                             (apply max (map island-size islands))))
      (sccs . ,(length sccs))
      (largest-scc . ,(if (null? sccs)
                          0
                          (apply max (map scc-size sccs))))
      (colors . ,(length colors))
      (max-parallel . ,(if (null? colors)
                           0
                           (apply max (map length colors)))))))

(doc 'cg-print-stats 'type '(-> ConstraintGraph Void))
(doc 'cg-print-stats 'description "Print human-readable constraint graph statistics")
(define (cg-print-stats graph)
  (let ([stats (cg-stats graph)])
    (printf "Constraint Graph Statistics:\n")
    (printf "  Entities:        ~a\n" (cdr (assq 'entities stats)))
    (printf "  Constraints:     ~a\n" (cdr (assq 'constraints stats)))
    (printf "  Islands:         ~a\n" (cdr (assq 'islands stats)))
    (printf "  Largest island:  ~a entities\n" (cdr (assq 'largest-island stats)))
    (printf "  SCCs:            ~a\n" (cdr (assq 'sccs stats)))
    (printf "  Largest SCC:     ~a entities\n" (cdr (assq 'largest-scc stats)))
    (printf "  Colors needed:   ~a\n" (cdr (assq 'colors stats)))
    (printf "  Max parallel:    ~a constraints\n" (cdr (assq 'max-parallel stats)))))

(printf "constraint-graph.ss loaded\n")
