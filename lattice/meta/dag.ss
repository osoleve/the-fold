;;; lattice/meta/dag.ss — DAG Navigation
;;;
;;; Tools for navigating the skill dependency DAG.
;;; Find dependencies, dependents, paths, and structural queries.
;;;
;;; This is Lattice code: pure, uses Core primitives.
;;;
;;; Usage:
;;;   (lattice-deps 'skill)            ; Direct dependencies
;;;   (lattice-deps-transitive 'skill) ; Full dependency closure
;;;   (lattice-uses 'skill)            ; Direct dependents
;;;   (lattice-uses-transitive 'skill) ; Full dependent closure
;;;   (lattice-path 'from 'to)         ; Path between skills
;;;   (lattice-roots)                  ; Tier 0 skills
;;;   (lattice-leaves)                 ; Skills with no dependents
;;;
;;; Dependencies:
;;;   lattice/meta/kg.ss

(load "lattice/meta/kg.ss")

;;; ============================================================
;;; Direct Dependencies
;;; ============================================================

;;; lattice-deps : Symbol -> (List Symbol)
;;; Get direct dependencies of a skill
(define (lattice-deps skill-name)
  (kg-deps skill-name))

;;; lattice-uses : Symbol -> (List Symbol)
;;; Get skills that directly depend on this skill
(define (lattice-uses skill-name)
  (kg-uses skill-name))

;;; ============================================================
;;; Transitive Closure
;;; ============================================================

;;; lattice-deps-transitive : Symbol -> (List Symbol)
;;; Get all dependencies (transitive closure)
(define (lattice-deps-transitive skill-name)
  (let loop ([to-visit (kg-deps skill-name)]
             [visited '()])
       (if (null? to-visit)
           (reverse visited)
           (let ([current (car to-visit)]
                 [rest (cdr to-visit)])
                (if (memq current visited)
                    (loop rest visited)
                    (loop (append (kg-deps current) rest)
                          (cons current visited)))))))

;;; lattice-uses-transitive : Symbol -> (List Symbol)
;;; Get all dependents (reverse transitive closure)
(define (lattice-uses-transitive skill-name)
  (let loop ([to-visit (kg-uses skill-name)]
             [visited '()])
       (if (null? to-visit)
           (reverse visited)
           (let ([current (car to-visit)]
                 [rest (cdr to-visit)])
                (if (memq current visited)
                    (loop rest visited)
                    (loop (append (kg-uses current) rest)
                          (cons current visited)))))))

;;; ============================================================
;;; Path Finding
;;; ============================================================

;;; lattice-path : Symbol Symbol -> (List Symbol) | #f
;;; Find a dependency path from one skill to another
;;; Returns path including both endpoints, or #f if no path exists
(define (lattice-path from to)
  (if (eq? from to)
      (list from)
      (let loop ([queue (list (list from))]
                 [visited '()])
           (if (null? queue)
               #f  ; No path found
               (let* ([path (car queue)]
                      [current (car path)]
                      [rest (cdr queue)])
                     (if (memq current visited)
                         (loop rest visited)
                         (let ([deps (kg-deps current)])
                              (if (memq to deps)
                                  (reverse (cons to path))  ; Found!
                                  (loop (append rest
                                                (map (lambda (d) (cons d path))
                                                     (filter (lambda (d) (not (memq d visited)))
                                                             deps)))
                                        (cons current visited))))))))))

;;; lattice-distance : Symbol Symbol -> Int | #f
;;; Get the shortest dependency distance between two skills
(define (lattice-distance from to)
  (let ([path (lattice-path from to)])
       (if path
           (- (length path) 1)
           #f)))

;;; ============================================================
;;; Structural Queries
;;; ============================================================

;;; lattice-roots : -> (List Symbol)
;;; Get tier 0 skills (no dependencies)
(define (lattice-roots)
  (filter
   (lambda (skill-name)
           (null? (kg-deps skill-name)))
   (kg-skills)))

;;; lattice-leaves : -> (List Symbol)
;;; Get skills with no dependents
(define (lattice-leaves)
  (filter
   (lambda (skill-name)
           (null? (kg-uses skill-name)))
   (kg-skills)))

;;; lattice-orphans : -> (List Symbol)
;;; Get exports that are not used anywhere
;;; (This is a placeholder - would need usage tracking)
(define (lattice-orphans)
  '())  ; TODO: Implement when usage tracking is added

;;; lattice-cycles : -> (List (List Symbol))
;;; Detect dependency cycles (should be empty in a valid DAG)
(define (lattice-cycles)
  (let ([cycles '()])
       (for-each
        (lambda (skill-name)
                (let ([trans-deps (lattice-deps-transitive skill-name)])
                     (when (memq skill-name trans-deps)
                           (set! cycles (cons skill-name cycles)))))
        (kg-skills))
       cycles))

;;; ============================================================
;;; Tier Analysis
;;; ============================================================

;;; lattice-tiers : -> ((tier . (skills ...)) ...)
;;; Group skills by their tier
(define (lattice-tiers)
  (let ([tier-map '()])
       (for-each
        (lambda (skill-name)
                (let* ([data (kg-skill-data skill-name)]
                       [tier (if data
                                 (let ([t (assq 'tier data)])
                                      (if t (cdr t) 0))
                                 0)]
                       [entry (assq tier tier-map)])
                      (if entry
                          (set-cdr! entry (cons skill-name (cdr entry)))
                          (set! tier-map (cons (cons tier (list skill-name)) tier-map)))))
        (kg-skills))
       (sort (lambda (a b) (< (car a) (car b))) tier-map)))

;;; lattice-depth : Symbol -> Int
;;; Get the maximum dependency depth of a skill
(define (lattice-depth skill-name)
  (let ([deps (kg-deps skill-name)])
       (if (null? deps)
           0
           (+ 1 (apply max (map lattice-depth deps))))))

;;; lattice-max-depth : -> Int
;;; Get the maximum depth of the entire DAG
(define (lattice-max-depth)
  (if (null? (kg-skills))
      0
      (apply max (map lattice-depth (kg-skills)))))

;;; ============================================================
;;; Impact Analysis
;;; ============================================================

;;; lattice-impact : Symbol -> Int
;;; Score based on how many skills depend on this (transitive)
(define (lattice-impact skill-name)
  (length (lattice-uses-transitive skill-name)))

;;; lattice-hubs : [Int] -> (List (Symbol . Int))
;;; Get most-depended-on skills (hubs)
(define (lattice-hubs . options)
  (let* ([k (if (pair? options) (car options) 10)]
         [impacts (map (lambda (s) (cons s (lattice-impact s))) (kg-skills))]
         [sorted (sort (lambda (a b) (> (cdr a) (cdr b))) impacts)])
        (take-at-most k sorted)))

;;; lattice-breaking-change? : Symbol Symbol -> Bool
;;; Would removing an export from a skill break anything?
;;; (Placeholder - would need export-level dependency tracking)
(define (lattice-breaking-change? skill-name export-name)
  ;; For now, assume any exported symbol might be used
  (> (lattice-impact skill-name) 0))

;;; ============================================================
;;; Visualization Helpers
;;; ============================================================

;;; lattice-tree : Symbol -> void
;;; Print dependency tree
(define (lattice-tree skill-name)
  (define (print-tree name indent)
    (printf "~a~a\n" (make-string (* indent 2) #\space) name)
    (for-each (lambda (dep) (print-tree dep (+ indent 1)))
              (kg-deps name)))
  (print-tree skill-name 0))

;;; lattice-reverse-tree : Symbol -> void
;;; Print dependent tree (what uses this)
(define (lattice-reverse-tree skill-name)
  (define (print-tree name indent)
    (printf "~a~a\n" (make-string (* indent 2) #\space) name)
    (for-each (lambda (dep) (print-tree dep (+ indent 1)))
              (kg-uses name)))
  (print-tree skill-name 0))

;;; lattice-graph : -> void
;;; Print entire DAG structure
(define (lattice-graph)
  (printf "Lattice Skill DAG\n")
  (printf "=================\n\n")
  (let ([tiers (lattice-tiers)])
       (for-each
        (lambda (tier-entry)
                (let ([tier (car tier-entry)]
                      [skills (cdr tier-entry)])
                     (printf "Tier ~a:\n" tier)
                     (for-each
                      (lambda (skill)
                              (let ([deps (kg-deps skill)])
                                   (if (null? deps)
                                       (printf "  ~a\n" skill)
                                       (printf "  ~a -> ~a\n" skill deps))))
                      skills)
                     (printf "\n")))
        tiers)))

;;; take-at-most helper
(define (take-at-most n lst)
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (take-at-most (- n 1) (cdr lst)))))

;;; ============================================================
;;; Convenience Functions (for REPL)
;;; ============================================================

;;; ld : Symbol -> void
;;; Quick show dependencies
(define (ld skill-name)
  (let ([deps (kg-deps skill-name)])
       (if (null? deps)
           (printf "~a has no dependencies (tier 0)\n" skill-name)
           (printf "~a depends on: ~a\n" skill-name deps))))

;;; lu : Symbol -> void
;;; Quick show dependents
(define (lu skill-name)
  (let ([uses (kg-uses skill-name)])
       (if (null? uses)
           (printf "Nothing depends on ~a (leaf)\n" skill-name)
           (printf "~a is used by: ~a\n" skill-name uses))))

;;; ============================================================
;;; REPL Interface
;;; ============================================================

(printf "dag.ss loaded.\n")
(printf "  (lattice-deps 'skill)            - Direct dependencies\n")
(printf "  (lattice-deps-transitive 'skill) - Full dep closure\n")
(printf "  (lattice-uses 'skill)            - Direct dependents\n")
(printf "  (lattice-uses-transitive 'skill) - Full dep closure\n")
(printf "  (lattice-path 'from 'to)         - Find path\n")
(printf "  (lattice-roots)                  - Tier 0 skills\n")
(printf "  (lattice-leaves)                 - No dependents\n")
(printf "  (lattice-hubs)                   - Most used skills\n")
(printf "  (lattice-graph)                  - Print full DAG\n")
(printf "  (ld 'skill), (lu 'skill)         - Quick lookup\n")
