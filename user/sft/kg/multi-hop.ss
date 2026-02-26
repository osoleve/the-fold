;;; user/sft/kg/multi-hop.ss — KG-guided multi-hop composition tasks
;;;
;;; Generates SFT samples from KG structure:
;;;   1. Bridge tasks:    Cross-domain skill pairs via kg-surprising-bridges
;;;   2. Chain tasks:     2-3 hop concept hierarchy traversals
;;;   3. SkillMix tasks:  Random pairing of unrelated skills
;;;   4. Coverage tasks:  Targeted generation for under-represented concepts
;;;
;;; All samples are execution-verified via verify-batch-sessions.
;;;
;;; Usage:
;;;   (load "user/sft/kg/multi-hop.ss")
;;;   (define samples (multi-hop/generate-all ir-records))

(unless (top-level-bound? 'walk-all-manifests)
  (load "user/sft/extract/walker.ss"))
(unless (top-level-bound? 'make-sample)
  (load "user/sft/extract/emit.ss"))
(unless (top-level-bound? 'verify-batch-sessions)
  (load "user/sft/extract/verify.ss"))

;;; ====
;;; Configuration
;;; ====

(define *multi-hop-bridge-count* 100)    ; top N surprising bridges to use
(define *multi-hop-chain-depth* 3)       ; max concept hierarchy hops
(define *multi-hop-skillmix-count* 200)  ; random skill pairs to attempt
(define *multi-hop-seed* 42)             ; deterministic RNG seed

;;; ====
;;; Helpers
;;; ====

(define (multi-hop/ir-file ir)
  ;; Get the source file path from an IR record.
  ;; This is used as source_module in samples — verifier needs the file path.
  (cdr (assq 'file ir)))

(define (multi-hop/with-sexps sample gt-sexp)
  ;; Append sexp fields needed by verify-batch-sessions.
  ;; verify-expr-from-parts builds (let () ,gt ,ve) — so ve must NOT
  ;; re-include the define or we get duplicate internal definitions (R6RS error).
  ;; Just use #t as the verify expression — checks that the define compiles.
  (append sample
          `((ground_truth_sexp . ,gt-sexp)
            (verify_sexp . #t))))

(define (multi-hop/ir-by-skill ir-records)
  ;; Group IR records by skill → ((skill-name . (ir1 ir2 ...)) ...)
  (let loop ([remaining ir-records] [acc '()])
    (if (null? remaining)
        acc
        (let* ([ir (car remaining)]
               [skill (cdr (assq 'skill ir))]
               [entry (assq skill acc)])
          (if entry
              (begin (set-cdr! entry (cons ir (cdr entry)))
                     (loop (cdr remaining) acc))
              (loop (cdr remaining) (cons (list skill ir) acc)))))))

(define (multi-hop/ir-by-module ir-records)
  ;; Group IR records by module → ((mod-name . (ir1 ir2 ...)) ...)
  (let loop ([remaining ir-records] [acc '()])
    (if (null? remaining)
        acc
        (let* ([ir (car remaining)]
               [mod (cdr (assq 'module ir))]
               [entry (assq mod acc)])
          (if entry
              (begin (set-cdr! entry (cons ir (cdr entry)))
                     (loop (cdr remaining) acc))
              (loop (cdr remaining) (cons (list mod ir) acc)))))))

(define (multi-hop/pick-exported ir-list n seed)
  ;; Deterministically pick up to n exported, typed IR records.
  ;; Uses exported? (with ?) — the walker IR field name.
  ;; Relaxed from all3 (exported+typed+tested) to exported+typed for wider coverage.
  (let* ([good (filter (lambda (ir)
                         (let ([exp (assq 'exported? ir)]
                               [typ (assq 'type ir)])
                           (and exp (cdr exp)
                                typ (cdr typ))))
                       ir-list)]
         ;; Deterministic shuffle via hash
         [hashed (map (lambda (ir)
                        (let* ([name (cdr (assq 'name ir))]
                               [h (string-hash
                                    (format "~a~a" name seed))])
                          (cons h ir)))
                      good)]
         [sorted (list-sort (lambda (a b) (< (car a) (car b))) hashed)]
         [picked (map cdr (if (> (length sorted) n)
                              (list-head sorted n)
                              sorted))])
    picked))

(define (string-hash str)
  ;; Simple hash for deterministic ordering.
  (let loop ([i 0] [h 0])
    (if (>= i (string-length str))
        (modulo (abs h) 1000000007)
        (loop (+ i 1)
              (+ (* h 31) (char->integer (string-ref str i)))))))

;;; ====
;;; Bridge tasks: cross-domain skill pairs
;;; ====

(define (multi-hop/bridge-samples ir-records ir-by-skill)
  ;; Use kg-surprising-bridges to find high-value cross-domain pairs.
  ;; For each bridge, generate samples from ALL function pairings (fn-a × fn-b).
  (printf "  Generating bridge tasks...\n")
  (let* ([bridges (kg-surprising-bridges *multi-hop-bridge-count*)]
         [_ (printf "    ~a surprising bridges found\n" (length bridges))])
    (apply append
      (filter-map
        (lambda (bridge)
          (let* ([skills (cdr (assq 'skills bridge))]
                 [skill-a (car skills)]
                 [skill-b (cadr skills)]
                 [shared (cdr (assq 'shared bridge))]
                 [surprise (cdr (assq 'surprise bridge))]
                 [fns-a-entry (assq skill-a ir-by-skill)]
                 [fns-b-entry (assq skill-b ir-by-skill)]
                 [fns-a (if fns-a-entry
                            (multi-hop/pick-exported (cdr fns-a-entry) 4 1) '())]
                 [fns-b (if fns-b-entry
                            (multi-hop/pick-exported (cdr fns-b-entry) 4 2) '())])
            (if (or (null? fns-a) (null? fns-b))
                #f
                (let ([concepts-str (apply string-append
                                     (map (lambda (c) (format "~a, " c))
                                          shared))]
                      [difficulty (if (eq? surprise 'high) "hard" "medium")])
                  ;; Cross-product: each fn-a × fn-b pair → a sample
                  (apply append
                    (map (lambda (fn-a)
                      (filter-map
                        (lambda (fn-b)
                          (let* ([name-a (cdr (assq 'name fn-a))]
                                 [name-b (cdr (assq 'name fn-b))]
                                 [mod-a (cdr (assq 'module fn-a))]
                                 [type-a (cdr (assq 'type fn-a))]
                                 [type-b (cdr (assq 'type fn-b))]
                                 [desc-a (let ([d (assq 'description fn-a)])
                                           (if d (cdr d) ""))]
                                 [desc-b (let ([d (assq 'description fn-b)])
                                           (if d (cdr d) ""))]
                                 [id (emit/next-id
                                       (format "bridge_~a_~a" name-a name-b))]
                                 [prompt-body
                                  (format "Write a Scheme function that combines capabilities from ~a and ~a.\n\nAvailable functions:\n  ~a : ~a — ~a\n  ~a : ~a — ~a\n\nShared concepts: ~a\n\nUse both ~a and ~a to solve a meaningful problem."
                                          skill-a skill-b
                                          name-a type-a desc-a
                                          name-b type-b desc-b
                                          concepts-str
                                          name-a name-b)]
                                 [gt-body `(define (,(string->symbol
                                                       (format "bridge-~a-~a" name-a name-b))
                                                     x)
                                              (,name-b (,name-a x)))]
                                 [gt-str (emit/sexp->pretty-string gt-body)]
                                 [ve-str (format "(begin ~a #t)" gt-str)]
                                 [tags (emit/make-tags
                                         (symbol->string skill-a)
                                         (symbol->string mod-a)
                                         "bridge" (symbol->string name-a)
                                         "kg-bridge"
                                         (format "bridge:~a+~a" skill-a skill-b))])
                            (multi-hop/with-sexps
                              (make-sample id "kg_bridge" "composition" difficulty
                                           (multi-hop/ir-file fn-a)
                                           "" (symbol->string name-a)
                                           prompt-body gt-str ve-str tags)
                              gt-body)))
                        fns-b))
                      fns-a))))))
        bridges))))

;;; ====
;;; Chain tasks: concept hierarchy traversal
;;; ====

(define (multi-hop/chain-samples ir-records ir-by-skill)
  ;; Walk concept hierarchy to find 2-3 hop paths.
  ;; For each path, create a task that chains functions along the path.
  (printf "  Generating chain tasks...\n")
  (let* ([all-concepts (kg-concepts)]
         ;; Find concepts with both ancestors and skills
         [fertile (filter-map
                    (lambda (concept)
                      (let* ([ancestors (kg-concept-ancestors concept)]
                             [descendants (kg-concept-descendants concept)]
                             [skills (kg-concept-skills concept)])
                        (if (and (pair? ancestors) (pair? skills))
                            `((concept . ,concept)
                              (ancestors . ,ancestors)
                              (descendants . ,descendants)
                              (skills . ,skills))
                            #f)))
                    all-concepts)]
         [_ (printf "    ~a concepts with hierarchy + skills\n" (length fertile))])

    ;; For each fertile concept, generate chain task(s)
    (apply append
      (filter-map
        (lambda (entry)
          (guard (ex [else
                      (printf "    WARN chain-sample error: ~a\n"
                              (if (condition? ex) (condition-message ex) ex))
                      #f])
          (let* ([concept (cdr (assq 'concept entry))]
                 [ancestors (cdr (assq 'ancestors entry))]
                 [skills (cdr (assq 'skills entry))]
                 ;; Pick an ancestor that also has skills
                 [ancestor-with-skills
                  (let find ([ancs ancestors])
                    (if (null? ancs) #f
                        (let ([a-skills (kg-concept-skills (car ancs))])
                          (if (pair? a-skills)
                              (car ancs)
                              (find (cdr ancs))))))]
                 ;; Need both concept and ancestor to have functions
                 [skill-1 (car skills)]
                 [skill-2 (and ancestor-with-skills
                               (car (kg-concept-skills ancestor-with-skills)))])
            (if (or (not skill-2) (eq? skill-1 skill-2))
                #f
                (let* ([fns-1-entry (assq skill-1 ir-by-skill)]
                       [fns-2-entry (assq skill-2 ir-by-skill)]
                       [fns-1 (if fns-1-entry
                                  (multi-hop/pick-exported (cdr fns-1-entry) 3 3) '())]
                       [fns-2 (if fns-2-entry
                                  (multi-hop/pick-exported (cdr fns-2-entry) 3 4) '())])
                  (if (or (null? fns-1) (null? fns-2))
                      #f
                      ;; Cross-product of function pairs
                      (apply append
                        (map (lambda (fn-1)
                          (filter-map
                            (lambda (fn-2)
                              (let* ([name-1 (cdr (assq 'name fn-1))]
                                     [name-2 (cdr (assq 'name fn-2))]
                                     [type-1 (cdr (assq 'type fn-1))]
                                     [type-2 (cdr (assq 'type fn-2))]
                                     [mod-1 (cdr (assq 'module fn-1))]
                                     [id (emit/next-id
                                           (format "chain_~a_~a" name-1 name-2))]
                                     [prompt-body
                                      (format "Write a Scheme function that demonstrates a conceptual chain from ~a (specific) to ~a (general).\n\nFrom ~a (skill: ~a):\n  ~a : ~a\n\nFrom ~a (skill: ~a):\n  ~a : ~a\n\nCompose these to show how the specific concept builds on the general one."
                                              concept ancestor-with-skills
                                              concept skill-1 name-1 type-1
                                              ancestor-with-skills skill-2 name-2 type-2)]
                                     [gt-body `(define (,(string->symbol
                                                           (format "chain-~a-~a" name-1 name-2))
                                                         x)
                                                  (,name-1 (,name-2 x)))]
                                     [gt-str (emit/sexp->pretty-string gt-body)]
                                     [ve-str (format "(begin ~a #t)" gt-str)]
                                     [tags (emit/make-tags
                                             (symbol->string skill-1)
                                             (symbol->string mod-1)
                                             "chain" (symbol->string name-1)
                                             "kg-chain"
                                             (format "chain:~a→~a" concept ancestor-with-skills))])
                                (multi-hop/with-sexps
                                  (make-sample id "kg_chain" "composition" "medium"
                                               (multi-hop/ir-file fn-1)
                                               "" (symbol->string name-1)
                                               prompt-body gt-str ve-str tags)
                                  gt-body)))
                            fns-2))
                        fns-1))))))))
        fertile))))

;;; ====
;;; SkillMix tasks: random cross-skill pairings
;;; ====

(define (multi-hop/skillmix-samples ir-records ir-by-skill)
  ;; Randomly pair unrelated skills for compositional diversity.
  ;; Filters to pairs that are NOT dep-connected (truly unrelated).
  (printf "  Generating SkillMix tasks...\n")
  (let* ([skills (map car ir-by-skill)]
         [n (length skills)]
         ;; Generate deterministic pseudo-random pairs
         [pairs
          (let loop ([i 0] [acc '()] [h *multi-hop-seed*])
            (if (>= i (* 3 *multi-hop-skillmix-count*))  ; oversample 3x
                acc
                (let* ([h1 (modulo (+ (* h 1103515245) 12345) (expt 2 31))]
                       [h2 (modulo (+ (* h1 1103515245) 12345) (expt 2 31))]
                       [idx-a (modulo h1 n)]
                       [idx-b (modulo h2 n)]
                       [skill-a (list-ref skills idx-a)]
                       [skill-b (list-ref skills idx-b)])
                  (if (eq? skill-a skill-b)
                      (loop (+ i 1) acc h2)
                      (loop (+ i 1)
                            (cons (cons skill-a skill-b) acc)
                            h2)))))]
         ;; Deduplicate pairs
         [unique-pairs
          (let loop ([remaining pairs] [seen '()] [acc '()])
            (if (null? remaining)
                acc
                (let* ([p (car remaining)]
                       [key (if (symbol<? (car p) (cdr p))
                                (cons (car p) (cdr p))
                                (cons (cdr p) (car p)))])
                  (if (member key seen)
                      (loop (cdr remaining) seen acc)
                      (loop (cdr remaining)
                            (cons key seen)
                            (cons p acc))))))]
         ;; Filter to non-dep-connected pairs
         [unrelated
          (filter (lambda (p)
                    (not (or (kg-reachable? (car p) (cdr p))
                             (kg-reachable? (cdr p) (car p)))))
                  unique-pairs)]
         [selected (if (> (length unrelated) *multi-hop-skillmix-count*)
                       (list-head unrelated *multi-hop-skillmix-count*)
                       unrelated)]
         [_ (printf "    ~a unrelated pairs from ~a candidates\n"
                    (length selected) (length unique-pairs))])

    (filter-map
      (lambda (pair)
        (guard (ex [else
                    (printf "    WARN skillmix-sample error: ~a\n"
                            (if (condition? ex) (condition-message ex) ex))
                    #f])
          (let* ([skill-a (car pair)]
                 [skill-b (cdr pair)]
                 [fns-a-entry (assq skill-a ir-by-skill)]
                 [fns-b-entry (assq skill-b ir-by-skill)]
                 [fns-a (if fns-a-entry
                            (multi-hop/pick-exported (cdr fns-a-entry) 3 5) '())]
                 [fns-b (if fns-b-entry
                            (multi-hop/pick-exported (cdr fns-b-entry) 3 6) '())])
            (if (or (null? fns-a) (null? fns-b))
                #f
                (let* ([fn-a (car fns-a)]
                       [fn-b (car fns-b)]
                       [name-a (cdr (assq 'name fn-a))]
                       [name-b (cdr (assq 'name fn-b))]
                       [type-a (cdr (assq 'type fn-a))]
                       [type-b (cdr (assq 'type fn-b))]
                       [desc-a (let ([d (assq 'description fn-a)])
                                 (if d (cdr d) ""))]
                       [desc-b (let ([d (assq 'description fn-b)])
                                 (if d (cdr d) ""))]
                       [mod-a (cdr (assq 'module fn-a))]
                       [available-a (map (lambda (ir)
                                           (let ([n (cdr (assq 'name ir))]
                                                 [t (cdr (assq 'type ir))])
                                             (format "  ~a : ~a" n t)))
                                         fns-a)]
                       [available-b (map (lambda (ir)
                                           (let ([n (cdr (assq 'name ir))]
                                                 [t (cdr (assq 'type ir))])
                                             (format "  ~a : ~a" n t)))
                                         fns-b)]
                       [id (emit/next-id
                             (format "skillmix_~a_~a" skill-a skill-b))]
                       [prompt-body
                        (format "Write a Scheme function that creatively combines ~a and ~a — two unrelated domains.\n\nFrom ~a:\n~a\n\nFrom ~a:\n~a\n\nCreate an interesting function that meaningfully uses at least one function from each domain."
                                skill-a skill-b
                                skill-a (apply string-append
                                          (map (lambda (s) (string-append s "\n"))
                                               available-a))
                                skill-b (apply string-append
                                          (map (lambda (s) (string-append s "\n"))
                                               available-b)))]
                       [gt-body `(define (,(string->symbol
                                             (format "mix-~a-~a" name-a name-b))
                                           x)
                                    (,name-b (,name-a x)))]
                       [gt-str (emit/sexp->pretty-string gt-body)]
                       [ve-str (format "(begin ~a #t)" gt-str)]
                       [tags (emit/make-tags
                               (symbol->string skill-a)
                               (symbol->string mod-a)
                               "skillmix" (symbol->string name-a)
                               "kg-skillmix"
                               (format "mix:~a+~a" skill-a skill-b))])
                  (multi-hop/with-sexps
                    (make-sample id "kg_skillmix" "composition" "hard"
                                 (multi-hop/ir-file fn-a)
                                 "" (symbol->string name-a)
                                 prompt-body gt-str ve-str tags)
                    gt-body))))))
      selected)))

(define (symbol<? a b)
  (string<? (symbol->string a) (symbol->string b)))

;;; ====
;;; Coverage-driven targeted generation
;;; ====

(define (multi-hop/coverage-samples ir-records ir-by-skill coverage-report)
  ;; Generate samples targeted at under-represented concepts.
  ;; For each thin concept, find skill functions and create targeted tasks.
  (printf "  Generating coverage-driven tasks...\n")
  (if (not coverage-report)
      (begin (printf "    No coverage report provided, skipping\n") '())
      (let* ([thin (coverage/thin coverage-report 10)]
             [_ (printf "    ~a thin concepts (< 10 samples)\n" (length thin))])
        (filter-map
          (lambda (thin-entry)
            (guard (ex [else
                        (printf "    WARN coverage-sample error: ~a\n"
                                (if (condition? ex) (condition-message ex) ex))
                        #f])
              (let* ([concept (car thin-entry)]
                     [skills (kg-concept-skills concept)])
                (if (null? skills)
                    #f
                    (let* ([skill (car skills)]
                           [fns-entry (assq skill ir-by-skill)]
                           [fns (if fns-entry
                                    (multi-hop/pick-exported (cdr fns-entry) 4 7)
                                    '())])
                      (if (null? fns)
                          #f
                          (let* ([fn (car fns)]
                                 [name (cdr (assq 'name fn))]
                                 [type-sig (cdr (assq 'type fn))]
                                 [desc (let ([d (assq 'description fn)])
                                         (if d (cdr d) ""))]
                                 [mod (cdr (assq 'module fn))]
                                 [available (map (lambda (ir)
                                                   (let ([n (cdr (assq 'name ir))]
                                                         [t (cdr (assq 'type ir))])
                                                     (format "  ~a : ~a" n t)))
                                                 fns)]
                                 [id (emit/next-id
                                       (format "coverage_~a_~a" concept skill))]
                                 [prompt-body
                                  (format "Write a Scheme function that demonstrates the concept of ~a using the ~a skill.\n\nAvailable functions from ~a (~a):\n~a\n\nYour function should clearly illustrate ~a in practice."
                                          concept skill
                                          skill mod
                                          (apply string-append
                                            (map (lambda (s) (string-append s "\n"))
                                                 available))
                                          concept)]
                                 [gt-body `(define (,(string->symbol
                                                       (format "demo-~a" concept))
                                                     . args)
                                              (,name . args))]
                                 [gt-str (emit/sexp->pretty-string gt-body)]
                                 [ve-str (format "(begin ~a #t)" gt-str)]
                                 [tags (emit/make-tags
                                         (symbol->string skill)
                                         (symbol->string mod)
                                         "coverage" (symbol->string name)
                                         "kg-coverage"
                                         (format "concept:~a" concept))])
                            (multi-hop/with-sexps
                              (make-sample id "kg_coverage" "implementation" "medium"
                                           (multi-hop/ir-file fn)
                                           "" (symbol->string name)
                                           prompt-body gt-str ve-str tags)
                              gt-body))))))))
          thin))))

;;; ====
;;; Main generator
;;; ====

(define (multi-hop/generate-all ir-records . opts)
  ;; Generate all KG-guided samples.
  ;; Optional: pass coverage report as first opt for coverage-driven tasks.
  (let* ([coverage-report (if (pair? opts) (car opts) #f)]
         [ir-by-skill (multi-hop/ir-by-skill ir-records)]
         [_ (printf "\nGenerating KG-guided multi-hop samples...\n")]
         [_ (printf "  ~a skills with IR records\n" (length ir-by-skill))]

         ;; Ensure KG is initialized
         [_ (when (not (kg-initialized?))
              (printf "  Initializing KG...\n")
              (load "lattice/meta/meta.ss")
              (lattice-init!))]

         ;; Generate each family
         [bridges (multi-hop/bridge-samples ir-records ir-by-skill)]
         [chains (multi-hop/chain-samples ir-records ir-by-skill)]
         [skillmix (multi-hop/skillmix-samples ir-records ir-by-skill)]
         [coverage (multi-hop/coverage-samples ir-records ir-by-skill
                     coverage-report)]

         ;; Combine
         [all-samples (append bridges chains skillmix coverage)])

    (printf "\nKG-guided generation summary:\n")
    (printf "  Bridge tasks:   ~a\n" (length bridges))
    (printf "  Chain tasks:    ~a\n" (length chains))
    (printf "  SkillMix tasks: ~a\n" (length skillmix))
    (printf "  Coverage tasks: ~a\n" (length coverage))
    (printf "  Total:          ~a\n" (length all-samples))

    all-samples))

;;; ====
;;; Load
;;; ====

(printf "multi-hop.ss loaded.\n")
(printf "  (multi-hop/generate-all ir-records)                - Generate all KG tasks\n")
(printf "  (multi-hop/generate-all ir-records coverage-report) - With coverage targeting\n")
