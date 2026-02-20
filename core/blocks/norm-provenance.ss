(load "core/blocks/normalize.ss")

(doc 'module 'norm-provenance)
(doc 'description "Normalization provenance traces. Wraps the normalization pipeline
to capture per-phase provenance: what went in, what came out, whether
anything changed, and what category of rules could have fired. Supports
v1, v2, and v3 normalization. Pure analysis — no side effects.")
(doc 'layer 'core)

;;; ====
;;; Phase Records
;;; ====

(doc 'section 'phase-records)

(doc 'type '(-> Symbol Any Any (List Symbol) Alist))
(doc 'description "Create a phase record capturing one normalization step.")
(define (make-phase-record name before after rules)
  (list 'phase
        (list 'name name)
        (list 'before before)
        (list 'after after)
        (list 'changed? (not (equal? before after)))
        (list 'rules rules)))

(doc 'type '(-> Any Boolean))
(define (phase-record? x)
  (and (pair? x) (eq? (car x) 'phase)))

(doc 'type '(-> Alist Symbol Any))
(doc 'description "Access a field from a phase record.")
(define (phase-field rec key)
  (let loop ([fields (cdr rec)])
    (cond
      [(null? fields) #f]
      [(and (pair? (car fields)) (eq? (caar fields) key))
       (cadar fields)]
      [else (loop (cdr fields))])))

;;; ====
;;; Rule Detection
;;; ====

(doc 'section 'rule-detection)

(doc 'type '(-> Any Any (List Symbol)))
(doc 'description "Detect which algebraic rules fired by comparing before/after.")
(define (detect-algebraic-rules before after)
  (if (equal? before after)
      '()
      (let ([rules '()])
        ;; Check for associative flattening (nested ops became flat)
        (let ([rules (if (and (pair? before) (pair? after)
                              (symbol? (car before)) (eq? (car before) (car after))
                              (any-nested-op? (car before) (cdr before))
                              (not (any-nested-op? (car after) (cdr after))))
                         (cons 'associative-flatten rules)
                         rules)])
          ;; Check for commutative sort (same elements, different order)
          (let ([rules (if (and (pair? before) (pair? after)
                                (eq? (car before) (car after))
                                (not (equal? (cdr before) (cdr after)))
                                (equal? (list-sort expr<? (cdr before))
                                        (list-sort expr<? (cdr after))))
                           (cons 'commutative-sort rules)
                           rules)])
            ;; Check for let* reordering
            (let ([rules (if (and (pair? before) (pair? after)
                                  (eq? (car before) 'let*) (eq? (car after) 'let*))
                             (cons 'binding-reorder rules)
                             rules)])
              ;; Check for begin sorting
              (let ([rules (if (and (pair? before) (pair? after)
                                    (eq? (car before) 'begin) (eq? (car after) 'begin)
                                    (not (equal? (cdr before) (cdr after))))
                               (cons 'begin-sort rules)
                               rules)])
                (if (null? rules)
                    '(algebraic-transform)  ; changed but couldn't identify specific rule
                    rules))))))))

(doc 'type '(-> Symbol (List Any) Boolean))
(doc 'description "Check if any argument contains a nested application of the same operator.")
(define (any-nested-op? op args)
  (let loop ([args args])
    (cond
      [(null? args) #f]
      [(and (pair? (car args)) (eq? (caar args) op)) #t]
      [else (loop (cdr args))])))

(doc 'type '(-> Any Any Boolean))
(doc 'description "Ordering for expression comparison in rule detection.")
(define (expr<? a b)
  (string<? (format "~s" a) (format "~s" b)))

(doc 'type '(-> Any Any (List Symbol)))
(doc 'description "Detect which alpha-normalization rules fired.")
(define (detect-alpha-rules before after)
  (if (equal? before after)
      '()
      (let ([rules '()])
        ;; Check for de Bruijn introduction
        (let ([rules (if (contains-dv? after)
                         (cons 'de-bruijn-index rules)
                         rules)])
          ;; Check for doc stripping
          (let ([rules (if (and (contains-doc? before)
                                (not (contains-doc? after)))
                           (cons 'doc-strip rules)
                           rules)])
            ;; Check for begin unwrap
            (let ([rules (if (and (pair? before) (eq? (car before) 'begin)
                                  (not (and (pair? after) (eq? (car after) 'begin))))
                             (cons 'begin-unwrap rules)
                             rules)])
              (if (null? rules)
                  '(alpha-rename)
                  rules)))))))

(doc 'type '(-> Any Any (List Symbol)))
(doc 'description "Detect which eta-reduction rules fired.")
(define (detect-eta-rules before after)
  (if (equal? before after)
      '()
      '(eta-reduce)))

(doc 'type '(-> Any Any (List Symbol)))
(doc 'description "Detect which identity elimination rules fired.")
(define (detect-identity-rules before after)
  (if (equal? before after)
      '()
      (let ([rules '()])
        ;; Check for absorbing element
        (let ([rules (if (and (pair? before) (not (pair? after))
                              (or (eqv? after 0) (eqv? after 1)))
                         (cons 'absorbing-element rules)
                         rules)])
          ;; Check for identity removal (args list got shorter)
          (let ([rules (if (and (pair? before) (pair? after)
                                (symbol? (car before)) (eq? (car before) (car after))
                                (< (length (cdr after)) (length (cdr before))))
                           (cons 'identity-removal rules)
                           rules)])
            (if (null? rules)
                '(identity-elim)
                rules))))))

(doc 'type '(-> Any Any (List Symbol)))
(doc 'description "Detect which NbE rules fired.")
(define (detect-nbe-rules before after)
  (if (equal? before after)
      '()
      (let ([rules '()])
        ;; Check for beta reduction (application eliminated)
        (let ([rules (if (and (pair? before)
                              (pair? (car before))
                              (eq? (caar before) 'fn))
                         (cons 'beta-reduce rules)
                         rules)])
          ;; Check for pair projection
          (let ([rules (if (and (pair? before)
                                (memq (car before) '(fst snd)))
                           (cons 'pair-project rules)
                           rules)])
            ;; Check for case reduction
            (let ([rules (if (and (pair? before)
                                  (eq? (car before) 'case)
                                  (not (and (pair? after) (eq? (car after) 'case))))
                             (cons 'case-reduce rules)
                             rules)])
              ;; Check for conditional reduction
              (let ([rules (if (and (pair? before)
                                    (eq? (car before) 'if)
                                    (not (and (pair? after) (eq? (car after) 'if))))
                               (cons 'conditional-reduce rules)
                               rules)])
                (if (null? rules)
                    '(nbe-normalize)
                    rules))))))))

(doc 'type '(-> Any Boolean))
(define (contains-dv? expr)
  (cond
    [(not (pair? expr)) #f]
    [(eq? (car expr) 'dv) #t]
    [else (or (contains-dv? (car expr))
              (contains-dv? (cdr expr)))]))

(doc 'type '(-> Any Boolean))
(define (contains-doc? expr)
  (cond
    [(not (pair? expr)) #f]
    [(eq? (car expr) 'doc) #t]
    [else (or (contains-doc? (car expr))
              (contains-doc? (cdr expr)))]))

;;; ====
;;; Provenance Traces
;;; ====

(doc 'section 'provenance-traces)

(doc 'type '(-> Symbol Any Any (List Alist) Alist))
(doc 'description "Create a normalization provenance trace.")
(define (make-norm-trace version input output phases)
  (list 'norm-trace
        (list 'version version)
        (list 'input input)
        (list 'output output)
        (list 'changed? (not (equal? input output)))
        (list 'phase-count (length phases))
        (list 'active-phases (length (filter (lambda (p) (phase-field p 'changed?)) phases)))
        (list 'phases phases)))

(doc 'type '(-> Any Boolean))
(define (norm-trace? x)
  (and (pair? x) (eq? (car x) 'norm-trace)))

(doc 'type '(-> Alist Symbol Any))
(doc 'description "Access a field from a norm-trace.")
(define (norm-trace-field trace key)
  (let loop ([fields (cdr trace)])
    (cond
      [(null? fields) #f]
      [(and (pair? (car fields)) (eq? (caar fields) key))
       (cadar fields)]
      [else (loop (cdr fields))])))

;;; ====
;;; Version-Specific Provenance
;;; ====

(doc 'section 'versioned-provenance)

(doc 'type '(-> Any Alist))
(doc 'description "Run v1 normalization (algebraic + alpha) with provenance tracking.")
(define (normalize-v1-provenance expr)
  (let* ([after-alg (normalize-algebraic expr)]
         [alg-phase (make-phase-record 'algebraic expr after-alg
                      (detect-algebraic-rules expr after-alg))]
         [after-alpha (normalize after-alg)]
         [alpha-phase (make-phase-record 'alpha after-alg after-alpha
                        (detect-alpha-rules after-alg after-alpha))])
    (make-norm-trace 'v1 expr after-alpha
                     (list alg-phase alpha-phase))))

(doc 'type '(-> Any Alist))
(doc 'description "Run v2 normalization (eta + poly-canon + algebraic + identity + alpha + hash-cons) with provenance.")
(define (normalize-v2-provenance expr)
  (let* ([after-eta (eta-reduce expr)]
         [eta-phase (make-phase-record 'eta-reduce expr after-eta
                      (detect-eta-rules expr after-eta))]

         [after-poly (poly-canonicalize-recursive after-eta)]
         [poly-phase (make-phase-record 'poly-canon after-eta after-poly
                       (if (equal? after-eta after-poly) '() '(poly-canonicalize)))]

         [after-alg (normalize-algebraic after-poly)]
         [alg-phase (make-phase-record 'algebraic after-poly after-alg
                      (detect-algebraic-rules after-poly after-alg))]

         [after-ident (eliminate-identities after-alg)]
         [ident-phase (make-phase-record 'identity-elim after-alg after-ident
                        (detect-identity-rules after-alg after-ident))]

         [after-alpha (normalize after-ident)]
         [alpha-phase (make-phase-record 'alpha after-ident after-alpha
                        (detect-alpha-rules after-ident after-alpha))]

         [after-hc (hash-cons after-alpha)]
         [hc-phase (make-phase-record 'hash-cons after-alpha after-hc
                     (if (eq? after-alpha after-hc) '() '(hash-cons-dedup)))])
    (make-norm-trace 'v2 expr after-hc
                     (list eta-phase poly-phase alg-phase
                           ident-phase alpha-phase hc-phase))))

(doc 'type '(-> Any Alist))
(doc 'description "Run v3 normalization (nbe + v2 pipeline) with provenance.")
(define (normalize-v3-provenance expr)
  (let* ([after-nbe (nbe-normalize-for-cas expr)]
         [nbe-phase (make-phase-record 'nbe expr after-nbe
                      (detect-nbe-rules expr after-nbe))]

         [after-eta (eta-reduce after-nbe)]
         [eta-phase (make-phase-record 'eta-reduce after-nbe after-eta
                      (detect-eta-rules after-nbe after-eta))]

         [after-poly (poly-canonicalize-recursive after-eta)]
         [poly-phase (make-phase-record 'poly-canon after-eta after-poly
                       (if (equal? after-eta after-poly) '() '(poly-canonicalize)))]

         [after-alg (normalize-algebraic after-poly)]
         [alg-phase (make-phase-record 'algebraic after-poly after-alg
                      (detect-algebraic-rules after-poly after-alg))]

         [after-ident (eliminate-identities after-alg)]
         [ident-phase (make-phase-record 'identity-elim after-alg after-ident
                        (detect-identity-rules after-alg after-ident))]

         [after-alpha (normalize after-ident)]
         [alpha-phase (make-phase-record 'alpha after-ident after-alpha
                        (detect-alpha-rules after-ident after-alpha))]

         [after-hc (hash-cons after-alpha)]
         [hc-phase (make-phase-record 'hash-cons after-alpha after-hc
                     (if (eq? after-alpha after-hc) '() '(hash-cons-dedup)))])
    (make-norm-trace 'v3 expr after-hc
                     (list nbe-phase eta-phase poly-phase alg-phase
                           ident-phase alpha-phase hc-phase))))

(doc 'type '(-> Any (U 'v1 'v2 'v3) Alist))
(doc 'description "Run normalization with provenance tracking for any version.")
(define (normalize-with-provenance expr version)
  (case version
    [(v1) (normalize-v1-provenance expr)]
    [(v2) (normalize-v2-provenance expr)]
    [(v3) (normalize-v3-provenance expr)]
    [else (error 'normalize-with-provenance
                 (format "Unknown version: ~a" version))]))

;;; ====
;;; Trace Analysis
;;; ====

(doc 'section 'trace-analysis)

(doc 'type '(-> Alist (List Symbol)))
(doc 'description "Collect all rules that fired across all phases.")
(define (trace-all-rules trace)
  (let ([phases (norm-trace-field trace 'phases)])
    (apply append (map (lambda (p) (phase-field p 'rules)) phases))))

(doc 'type '(-> Alist (List Alist)))
(doc 'description "Return only phases that changed the expression.")
(define (trace-active-phases trace)
  (filter (lambda (p) (phase-field p 'changed?))
          (norm-trace-field trace 'phases)))

(doc 'type '(-> Alist Alist Alist))
(doc 'description "Compare two provenance traces and report differences.
Returns a diff record showing where the traces diverge.")
(define (trace-compare t1 t2)
  (let* ([phases1 (norm-trace-field t1 'phases)]
         [phases2 (norm-trace-field t2 'phases)]
         [same-input? (equal? (norm-trace-field t1 'input)
                              (norm-trace-field t2 'input))]
         [same-output? (equal? (norm-trace-field t1 'output)
                               (norm-trace-field t2 'output))]
         [divergence-point
          (let loop ([p1 phases1] [p2 phases2] [idx 0])
            (cond
              [(or (null? p1) (null? p2)) #f]
              [(not (equal? (phase-field (car p1) 'after)
                            (phase-field (car p2) 'after)))
               (list 'diverge-at
                     (list 'phase-index idx)
                     (list 'phase-name (phase-field (car p1) 'name))
                     (list 'trace1-after (phase-field (car p1) 'after))
                     (list 'trace2-after (phase-field (car p2) 'after)))]
              [else (loop (cdr p1) (cdr p2) (+ idx 1))]))])
    (list 'trace-diff
          (list 'same-input? same-input?)
          (list 'same-output? same-output?)
          (list 'trace1-version (norm-trace-field t1 'version))
          (list 'trace2-version (norm-trace-field t2 'version))
          (list 'trace1-active (norm-trace-field t1 'active-phases))
          (list 'trace2-active (norm-trace-field t2 'active-phases))
          (list 'divergence divergence-point))))

(doc 'type '(-> Alist Alist))
(doc 'description "Produce a compact summary of a provenance trace.")
(define (trace-summary trace)
  (list 'norm-summary
        (list 'version (norm-trace-field trace 'version))
        (list 'changed? (norm-trace-field trace 'changed?))
        (list 'active-phases (norm-trace-field trace 'active-phases))
        (list 'total-phases (norm-trace-field trace 'phase-count))
        (list 'rules-fired (trace-all-rules trace))
        (list 'active-phase-names
              (map (lambda (p) (phase-field p 'name))
                   (trace-active-phases trace)))))
