;;; @module kg-bridge
;;; @description Cross-skill bridge detection — scoring, ranking, and caching.
;;; Loaded by kg.ss — requires KG state tables and concept query API.

;;; ====
;;; Bridge Detection (zy13)
;;; ====

(doc 'section 'bridge-detection)

;;; Helper: check if skill-a can reach skill-b through the dependency graph
(define (kg-reachable? from to)
  (let loop ([frontier (kg-deps from)] [visited (list from)])
    (cond
      [(null? frontier) #f]
      [(eq? (car frontier) to) #t]
      [(memq (car frontier) visited) (loop (cdr frontier) visited)]
      [else (loop (append (kg-deps (car frontier)) (cdr frontier))
                  (cons (car frontier) visited))])))

(doc kg-bridge-score 'type (-> Symbol Symbol Alist))
(doc kg-bridge-score 'description "Score the concept bridge between two skills.
Returns an alist with:
  score - overall bridge score (higher = more surprising overlap)
  shared - all shared concepts (direct + hierarchical)
  direct - concepts in both skills' flat concept lists
  hierarchical - concepts only reachable through hierarchy expansion
  cross-cutting - shared concepts flagged as cross-cutting
  dep-connected - 'direct, 'transitive, or 'none")
(define (kg-bridge-score skill-a skill-b)
  (if (eq? skill-a skill-b)
      '((score . 0) (shared) (direct) (hierarchical) (cross-cutting) (dep-connected . none))
      (let* ([concepts-a (kg-skill-concepts skill-a)]
             [concepts-b (kg-skill-concepts skill-b)]
             [shared (kg-shared-concepts-transitive skill-a skill-b)]
             ;; Classify each shared concept
             [direct-shared (filter (lambda (c)
                                      (and (memq c concepts-a) (memq c concepts-b)))
                                    shared)]
             [hierarchical (filter (lambda (c) (not (memq c direct-shared))) shared)]
             [cross-cutting (filter (lambda (c)
                                      (and (concept-ontology-loaded?)
                                           (concept-cross-cutting? c)))
                                    shared)]
             ;; Raw score: 1.0 per direct, 0.5 per hierarchical, 1.5 bonus per cross-cutting
             [raw-score (+ (* 1.0 (length direct-shared))
                           (* 0.5 (length hierarchical))
                           (* 1.5 (length cross-cutting)))])
        ;; Only compute dep connectivity when raw-score > 0 (avoids BFS on most pairs)
        (if (zero? raw-score)
            '((score . 0) (shared) (direct) (hierarchical) (cross-cutting) (dep-connected . none))
            (let* ([a-deps-on-b (memq skill-b (kg-deps skill-a))]
                   [b-deps-on-a (memq skill-a (kg-deps skill-b))]
                   [direct-dep (or a-deps-on-b b-deps-on-a)]
                   [transitive-dep (and (not direct-dep)
                                        (or (kg-reachable? skill-a skill-b)
                                            (kg-reachable? skill-b skill-a)))]
                   [dep-type (cond [direct-dep 'direct]
                                   [transitive-dep 'transitive]
                                   [else 'none])]
                   [score (cond [(eq? dep-type 'direct) (* raw-score 0.3)]
                                [(eq? dep-type 'transitive) (* raw-score 0.5)]
                                [else raw-score])])
              `((score . ,score)
                (shared . ,shared)
                (direct . ,direct-shared)
                (hierarchical . ,hierarchical)
                (cross-cutting . ,cross-cutting)
                (dep-connected . ,dep-type)))))))

(doc kg-detect-bridges 'type (-> (List Alist)))
(doc kg-detect-bridges 'description "Detect all concept bridges between skills, ranked by score.
Scans all unique skill pairs, scores their concept overlap, and returns
bridges with score > 0 sorted by score descending. Results are cached
and invalidated by kg-reset!.
Each entry: ((skills . (a b)) (score . N) (surprise . high|medium|low) ...)")
(define (kg-detect-bridges)
  (or *kg-bridge-cache*
      (let* ([skills (kg-skills)]
             [pairs (let outer ([remaining skills] [acc '()])
                      (if (null? remaining)
                          acc
                          (let inner ([others (cdr remaining)]
                                      [acc acc]
                                      [a (car remaining)])
                            (if (null? others)
                                (outer (cdr remaining) acc)
                                (inner (cdr others)
                                       (cons (cons a (car others)) acc)
                                       a)))))]
             [scored
              (filter-map
               (lambda (pair)
                 (let* ([a (car pair)]
                        [b (cdr pair)]
                        [bridge (kg-bridge-score a b)]
                        [score (cdr (assq 'score bridge))])
                   (if (> score 0)
                       (let ([surprise
                              (let ([dep (cdr (assq 'dep-connected bridge))])
                                (cond [(and (>= score 2.0) (eq? dep 'none)) 'high]
                                      [(and (>= score 1.0) (not (eq? dep 'direct))) 'medium]
                                      [else 'low]))])
                         `((skills . ,(list a b))
                           (score . ,score)
                           (surprise . ,surprise)
                           (shared . ,(cdr (assq 'shared bridge)))
                           (direct . ,(cdr (assq 'direct bridge)))
                           (hierarchical . ,(cdr (assq 'hierarchical bridge)))
                           (cross-cutting . ,(cdr (assq 'cross-cutting bridge)))
                           (dep-connected . ,(cdr (assq 'dep-connected bridge)))))
                       #f)))
               pairs)]
             [sorted (list-sort (lambda (a b)
                                  (> (cdr (assq 'score a))
                                     (cdr (assq 'score b))))
                                scored)])
        (set! *kg-bridge-cache* sorted)
        sorted)))

(doc kg-surprising-bridges 'type (-> Int (List Alist)))
(doc kg-surprising-bridges 'description "Get the top k most surprising concept bridges.
Filters to surprise = high or medium (excludes dep-connected low-surprise pairs).")
(define (kg-surprising-bridges k)
  (let* ([all (kg-detect-bridges)]
         [surprising (filter (lambda (b)
                               (let ([s (cdr (assq 'surprise b))])
                                 (or (eq? s 'high) (eq? s 'medium))))
                             all)])
    (if (<= (length surprising) k)
        surprising
        (let loop ([lst surprising] [n 0] [acc '()])
          (if (or (null? lst) (= n k))
              (reverse acc)
              (loop (cdr lst) (+ n 1) (cons (car lst) acc)))))))
