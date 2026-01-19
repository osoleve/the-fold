;;; lattice/meta/analytics.ss — Lattice Analytics
;;;
;;; Statistics, health checks, and coverage analysis for the skill lattice.
;;;
;;; This is Lattice code: pure (mostly), uses Core primitives.
;;;
;;; Usage:
;;;   (lattice-stats)       ; Overall statistics
;;;   (lattice-health)      ; Health check report
;;;   (lattice-coverage)    ; Coverage analysis
;;;   (lattice-complexity)  ; Complexity metrics
;;;
;;; Dependencies:
;;;   lattice/meta/kg.ss
;;;   lattice/meta/dag.ss

(load "lattice/meta/dag.ss")

;;; ====
;;; Overall Statistics
;;; ====

;;; lattice-stats : -> Alist
;;; Get overall lattice statistics
(define (lattice-stats)
  (let* ([skills (kg-skills)]
         [num-skills (length skills)]
         [num-modules (fold-left (lambda (acc s) (+ acc (length (kg-modules s)))) 0 skills)]
         [num-exports (length (kg-exports))]
         [num-deps (length *kg-deps*)]
         [roots (lattice-roots)]
         [leaves (lattice-leaves)]
         [max-depth (if (null? skills) 0 (lattice-max-depth))]
         [avg-deps (if (= num-skills 0) 0
                       (/ (apply + (map (lambda (s) (length (kg-deps s))) skills))
                          num-skills))])
        `((skills . ,num-skills)
          (modules . ,num-modules)
          (exports . ,num-exports)
          (dependencies . ,num-deps)
          (roots . ,(length roots))
          (leaves . ,(length leaves))
          (max-depth . ,max-depth)
          (avg-deps . ,(exact->inexact avg-deps)))))

;;; lattice-stats-pretty : -> void
;;; Print statistics in a nice format
(define (lattice-stats-pretty)
  (if (not (kg-initialized?))
      (begin
        (printf "Knowledge graph not initialized.\n")
        (printf "Run (lattice-init!) first.\n"))
      (let ([stats (lattice-stats)])
           (printf "Lattice Statistics\n")
           (printf "====\n")
           (printf "  Skills:       ~a\n" (cdr (assq 'skills stats)))
           (printf "  Modules:      ~a\n" (cdr (assq 'modules stats)))
           (printf "  Exports:      ~a\n" (cdr (assq 'exports stats)))
           (printf "  Dependencies: ~a\n" (cdr (assq 'dependencies stats)))
           (printf "  Roots (T0):   ~a\n" (cdr (assq 'roots stats)))
           (printf "  Leaves:       ~a\n" (cdr (assq 'leaves stats)))
           (printf "  Max Depth:    ~a\n" (cdr (assq 'max-depth stats)))
           (printf "  Avg Deps:     ~a\n" (round-to (cdr (assq 'avg-deps stats)) 2)))))

;;; round-to : Number Int -> Number
(define (round-to n places)
  (let ([factor (expt 10 places)])
       (/ (round (* n factor)) factor)))

;;; ====
;;; Health Checks
;;; ====

;;; lattice-health : -> HealthReport
;;; Run health checks on the lattice
(define (lattice-health)
  (let* ([issues '()]
         [warnings '()]
         [skills (kg-skills)])

        ;; Check for cycles (using new detailed validator)
        (let ([cycle-errors (lattice-validate-all)])
             (for-each
              (lambda (err)
                      (set! issues (cons err issues)))
              cycle-errors))
        
        ;; Check for missing manifests (skills referenced but not defined)
        (for-each
         (lambda (skill-name)
                 (let ([deps (kg-deps skill-name)])
                      (for-each
                       (lambda (dep)
                               (unless (memq dep skills)
                                       (set! issues (cons `(missing-dep ,skill-name ,dep) issues))))
                       deps)))
         skills)
        
        ;; Check for skills without descriptions
        (for-each
         (lambda (skill-name)
                 (let* ([data (kg-skill-data skill-name)]
                        [desc (if data (cdr (or (assq 'description data) '(description . ""))) "")])
                       (when (or (not desc) (string=? desc ""))
                             (set! warnings (cons `(no-description ,skill-name) warnings)))))
         skills)
        
        ;; Check for skills without keywords
        (for-each
         (lambda (skill-name)
                 (let* ([data (kg-skill-data skill-name)]
                        [keywords (if data (cdr (or (assq 'keywords data) '(keywords . ()))) '())])
                       (when (or (not keywords) (null? keywords))
                             (set! warnings (cons `(no-keywords ,skill-name) warnings)))))
         skills)
        
        ;; Check for excessive dependencies
        (for-each
         (lambda (skill-name)
                 (let ([deps (kg-deps skill-name)])
                      (when (> (length deps) 5)
                            (set! warnings (cons `(many-deps ,skill-name ,(length deps)) warnings)))))
         skills)
        
        ;; Build report
        (let ([healthy (and (null? issues) (null? warnings))])
             `((healthy . ,healthy)
               (issues . ,(reverse issues))
               (warnings . ,(reverse warnings))))))

;;; lattice-health-pretty : -> void
;;; Print health report
(define (lattice-health-pretty)
  (let* ([report (lattice-health)]
         [healthy (cdr (assq 'healthy report))]
         [issues (cdr (assq 'issues report))]
         [warnings (cdr (assq 'warnings report))])
        (printf "Lattice Health Report\n")
        (printf "====\n\n")
        
        (if healthy
            (printf "Status: HEALTHY\n\n")
            (printf "Status: ISSUES FOUND\n\n"))
        
        (unless (null? issues)
                (printf "Issues (~a):\n" (length issues))
                (for-each
                 (lambda (issue)
                         (cond
                          ;; New cycle format: (err (cycle from to path))
                          [(and (pair? issue) (eq? (car issue) 'err))
                           (let* ([data (cadr issue)]
                                  [from (cadr data)]
                                  [to (caddr data)]
                                  [path (cadddr data)])
                                 (printf "  ERROR: Cycle detected in ~a\n" from)
                                 (printf "         ~a -> ~a creates: ~a\n"
                                         from to (format-cycle-path path)))]
                          ;; Legacy cycle format
                          [(and (pair? issue) (eq? (car issue) 'cycles))
                           (printf "  ERROR: Dependency cycles detected: ~a\n" (cdr issue))]
                          ;; Missing dep format
                          [(and (pair? issue) (eq? (car issue) 'missing-dep))
                           (printf "  ERROR: ~a depends on missing skill: ~a\n"
                                   (cadr issue) (caddr issue))]
                          [else
                           (printf "  ERROR: ~a\n" issue)]))
                 issues)
                (printf "\n"))
        
        (unless (null? warnings)
                (printf "Warnings (~a):\n" (length warnings))
                (for-each
                 (lambda (warning)
                         (case (car warning)
                               [(no-description)
                                (printf "  WARN: ~a has no description\n" (cadr warning))]
                               [(no-keywords)
                                (printf "  WARN: ~a has no keywords\n" (cadr warning))]
                               [(many-deps)
                                (printf "  WARN: ~a has ~a dependencies (consider splitting)\n"
                                        (cadr warning) (caddr warning))]
                               [else
                                (printf "  WARN: ~a\n" warning)]))
                 warnings)
                (printf "\n"))))

;;; ====
;;; Coverage Analysis
;;; ====

;;; lattice-coverage : -> CoverageReport
;;; Analyze documentation/test coverage
(define (lattice-coverage)
  (let* ([skills (kg-skills)]
         [total (length skills)]
         [with-desc 0]
         [with-keywords 0]
         [with-aliases 0]
         [with-modules 0])
        
        (for-each
         (lambda (skill-name)
                 (let ([data (kg-skill-data skill-name)])
                      (when data
                            (let ([desc (cdr (or (assq 'description data) '(description . "")))]
                                  [kw (cdr (or (assq 'keywords data) '(keywords . ())))]
                                  [al (cdr (or (assq 'aliases data) '(aliases . ())))]
                                  [mods (kg-modules skill-name)])
                                 (when (and desc (not (string=? desc "")))
                                       (set! with-desc (+ 1 with-desc)))
                                 (when (and kw (not (null? kw)))
                                       (set! with-keywords (+ 1 with-keywords)))
                                 (when (and al (not (null? al)))
                                       (set! with-aliases (+ 1 with-aliases)))
                                 (unless (null? mods)
                                         (set! with-modules (+ 1 with-modules)))))))
         skills)
        
        `((total . ,total)
          (with-description . ,with-desc)
          (with-keywords . ,with-keywords)
          (with-aliases . ,with-aliases)
          (with-modules . ,with-modules)
          (description-pct . ,(if (= total 0) 0 (* 100 (/ with-desc total))))
          (keywords-pct . ,(if (= total 0) 0 (* 100 (/ with-keywords total))))
          (aliases-pct . ,(if (= total 0) 0 (* 100 (/ with-aliases total))))
          (modules-pct . ,(if (= total 0) 0 (* 100 (/ with-modules total)))))))

;;; lattice-coverage-pretty : -> void
;;; Print coverage report
(define (lattice-coverage-pretty)
  (let ([cov (lattice-coverage)])
       (printf "Lattice Coverage Report\n")
       (printf "====\n\n")
       (printf "Total Skills: ~a\n\n" (cdr (assq 'total cov)))
       (printf "Metadata Coverage:\n")
       (printf "  Descriptions: ~a/~a (~a%)\n"
               (cdr (assq 'with-description cov))
               (cdr (assq 'total cov))
               (round-to (cdr (assq 'description-pct cov)) 1))
       (printf "  Keywords:     ~a/~a (~a%)\n"
               (cdr (assq 'with-keywords cov))
               (cdr (assq 'total cov))
               (round-to (cdr (assq 'keywords-pct cov)) 1))
       (printf "  Aliases:      ~a/~a (~a%)\n"
               (cdr (assq 'with-aliases cov))
               (cdr (assq 'total cov))
               (round-to (cdr (assq 'aliases-pct cov)) 1))
       (printf "  Modules:      ~a/~a (~a%)\n"
               (cdr (assq 'with-modules cov))
               (cdr (assq 'total cov))
               (round-to (cdr (assq 'modules-pct cov)) 1))))

;;; ====
;;; Complexity Metrics
;;; ====

;;; skill-complexity : Symbol -> Alist
;;; Get complexity metrics for a skill
(define (skill-complexity skill-name)
  (let* ([data (kg-skill-data skill-name)]
         [modules (kg-modules skill-name)]
         [exports (filter (lambda (e)
                                  (let ([key (symbol->string (car e))])
                                       ;; Filter exports for this skill (rough match)
                                       #t))
                          (kg-exports))]
         [deps (kg-deps skill-name)]
         [uses (kg-uses skill-name)]
         [trans-deps (lattice-deps-transitive skill-name)]
         [trans-uses (lattice-uses-transitive skill-name)]
         [depth (lattice-depth skill-name)])
        `((name . ,skill-name)
          (modules . ,(length modules))
          (direct-deps . ,(length deps))
          (transitive-deps . ,(length trans-deps))
          (direct-uses . ,(length uses))
          (transitive-uses . ,(length trans-uses))
          (depth . ,depth)
          (impact . ,(length trans-uses)))))

;;; lattice-complexity-summary : -> void
;;; Print complexity summary for all skills
(define (lattice-complexity-summary)
  (printf "Skill Complexity Summary\n")
  (printf "====\n\n")
  (printf "~20a ~6a ~8a ~8a ~6a\n" "Skill" "Mods" "Deps" "Users" "Depth")
  (printf "~20a ~6a ~8a ~8a ~6a\n" "----" "----" "----" "----" "----")
  (for-each
   (lambda (skill-name)
           (let ([c (skill-complexity skill-name)])
                (printf "~20a ~6a ~8a ~8a ~6a\n"
                        skill-name
                        (cdr (assq 'modules c))
                        (cdr (assq 'transitive-deps c))
                        (cdr (assq 'transitive-uses c))
                        (cdr (assq 'depth c)))))
   (kg-skills)))

;;; ====
;;; Purity Analysis
;;; ====

;;; lattice-purity-report : -> void
;;; Report on purity of skills
(define (lattice-purity-report)
  (let ([total 0]
        [pure 0]
        [partial 0]
        [unknown 0])
       (for-each
        (lambda (skill-name)
                (let* ([data (kg-skill-data skill-name)]
                       [purity (if data
                                   (let ([p (assq 'purity data)])
                                        (if p (cdr p) 'unknown))
                                   'unknown)])
                      (set! total (+ 1 total))
                      (case purity
                            [(total) (set! pure (+ 1 pure))]
                            [(partial) (set! partial (+ 1 partial))]
                            [else (set! unknown (+ 1 unknown))])))
        (kg-skills))
       (printf "Purity Report\n")
       (printf "====\n\n")
       (printf "Total:    ~a skills\n" total)
       (printf "Pure:     ~a (~a%)\n" pure (round-to (* 100 (/ pure total)) 1))
       (printf "Partial:  ~a (~a%)\n" partial (round-to (* 100 (/ partial total)) 1))
       (printf "Unknown:  ~a (~a%)\n" unknown (round-to (* 100 (/ unknown total)) 1))))

;;; ====
;;; Convenience Functions
;;; ====

;;; ls : -> void
;;; Quick lattice stats (for REPL)
(define (ls)
  (lattice-stats-pretty))

;;; lh : -> void
;;; Quick health check (for REPL)
(define (lh)
  (lattice-health-pretty))

;;; ====
;;; REPL Interface
;;; ====

(meta-printf "analytics.ss loaded.\n")
(meta-printf "  (lattice-stats)               - Overall statistics\n")
(meta-printf "  (lattice-stats-pretty)        - Pretty stats\n")
(meta-printf "  (lattice-health)              - Health check\n")
(meta-printf "  (lattice-health-pretty)       - Pretty health report\n")
(meta-printf "  (lattice-coverage)            - Coverage analysis\n")
(meta-printf "  (lattice-coverage-pretty)     - Pretty coverage\n")
(meta-printf "  (skill-complexity 'skill)     - Skill complexity\n")
(meta-printf "  (lattice-purity-report)       - Purity analysis\n")
(meta-printf "  (ls), (lh)                    - Quick stats/health\n")
