;;; @module egraph/extract
;;; @requires prelude egraph/cost egraph/saturation
;;; lattice/egraph/extract.ss — Cost-Based E-Graph Extraction
;;;
;;; Extraction recovers a concrete term from an e-graph by choosing
;;; the minimum-cost e-node from each e-class. The extracted term is
;;; semantically equivalent to any other term in the same e-class.
;;;
;;; Algorithm:
;;; 1. Compute costs for all e-classes (dynamic programming)
;;; 2. For each class, select the e-node with minimum cost
;;; 3. Recursively extract children using their selected e-nodes
;;;
;;; This is Lattice code: pure extraction logic.

(require 'prelude)
(require 'egraph/cost)
(require 'egraph/saturation)

(doc 'module 'egraph/extract)
(doc 'description "Cost-based extraction from e-graphs")
(doc 'layer 'lattice)
(doc 'tier 1)
(doc 'purity 'total)

;;; ============================================================
;;; Extraction State
;;; ============================================================

(doc 'section 'state)

;;; Extraction state tracks the best e-node per class.
;;; This is computed once via dynamic programming, then used
;;; for all extractions.

(define extraction-state-tag 'extraction-state)

(define (make-extraction-state eg cost-model)
  (doc 'type (-> EGraph CostModel ExtractionState))
  (doc 'description "Create extraction state with precomputed best nodes.")
  (doc 'export #t)
  (let ([costs (compute-costs eg cost-model)]
        [best-nodes (make-hashtable equal-hash equal?)]
        [uf (egraph-uf eg)]
        [store (egraph-classes eg)]
        [node-cost-fn (cost-model-fn cost-model)])
    ;; Find best node for each class
    (for-each
     (lambda (root)
       (let ([best-node #f]
             [best-cost +inf.0])
         (for-each
          (lambda (node)
            (let ([c (compute-node-cost node costs node-cost-fn)])
              (when (< c best-cost)
                (set! best-cost c)
                (set! best-node node))))
          (eclass-get-nodes store root))
         (when best-node
           (hashtable-set! best-nodes root best-node))))
     (uf-roots uf))
    (vector extraction-state-tag eg costs best-nodes)))

(define (extraction-state? x)
  (doc 'type (-> Any Boolean))
  (doc 'description "Check if x is an extraction state.")
  (doc 'export #t)
  (and (vector? x)
       (>= (vector-length x) 4)
       (eq? (vector-ref x 0) extraction-state-tag)))

(define (state-egraph st) (vector-ref st 1))
(define (state-costs st) (vector-ref st 2))
(define (state-best-nodes st) (vector-ref st 3))

;;; ============================================================
;;; Core Extraction
;;; ============================================================

(doc 'section 'extract)

;;; extract : ExtractionState × ClassId → Term
;;; Extract the minimum-cost term from an e-class.
(define (extract state class-id)
  (doc 'type (-> ExtractionState ClassId Term))
  (doc 'description "Extract minimum-cost term from e-class.")
  (doc 'export #t)
  (let* ([eg (state-egraph state)]
         [root (egraph-find eg class-id)]
         [best-nodes (state-best-nodes state)]
         [best-node (hashtable-ref best-nodes root #f)])
    (if (not best-node)
        ;; Shouldn't happen in a well-formed e-graph
        (error 'extract "no best node for class" root)
        (let ([op (enode-op best-node)]
              [children (enode-children best-node)])
          (if (zero? (vector-length children))
              ;; Leaf: return the operator as-is
              op
              ;; Compound: recursively extract children
              (cons op
                    (map (lambda (i)
                           (extract state (vector-ref children i)))
                         (iota (vector-length children)))))))))

;;; extract-term : EGraph × Term × CostModel → Term
;;; Add a term to an e-graph and extract its optimal form.
;;; Useful for one-off optimizations.
(define (extract-term eg term cost-model)
  (doc 'type (-> EGraph Term CostModel Term))
  (doc 'description "Add term to e-graph and extract optimal equivalent.")
  (doc 'export #t)
  (let* ([class-id (egraph-add-term! eg term)]
         [state (make-extraction-state eg cost-model)])
    (extract state class-id)))

;;; extract-all : ExtractionState → (List (ClassId × Term))
;;; Extract optimal term for every root e-class.
(define (extract-all state)
  (doc 'type (-> ExtractionState (List (Pair ClassId Term))))
  (doc 'description "Extract optimal terms for all root e-classes.")
  (doc 'export #t)
  (let* ([eg (state-egraph state)]
         [uf (egraph-uf eg)])
    (map (lambda (root)
           (cons root (extract state root)))
         (uf-roots uf))))

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

(doc 'section 'convenience)

;;; optimize : Term × Rules × CostModel → Term
;;; The main entry point: saturate an e-graph with rules,
;;; then extract the optimal equivalent form.
(define (optimize term rules cost-model)
  (doc 'type (-> Term (List Rule) CostModel Term))
  (doc 'description "Apply rules via equality saturation, extract optimal form.")
  (doc 'export #t)
  (let ([eg (make-egraph)])
    (let ([root (egraph-add-term! eg term)])
      (saturate-simple eg rules)
      (let ([state (make-extraction-state eg cost-model)])
        (extract state root)))))

;;; optimize-with-config : Term × Rules × CostModel × SaturationConfig → Term
;;; Optimize with custom saturation configuration.
(define (optimize-with-config term rules cost-model config)
  (doc 'type (-> Term (List Rule) CostModel SaturationConfig Term))
  (doc 'description "Optimize with custom saturation config.")
  (doc 'export #t)
  (let ([eg (make-egraph)])
    (let ([root (egraph-add-term! eg term)])
      (saturate eg rules config)
      (let ([state (make-extraction-state eg cost-model)])
        (extract state root)))))

;;; ============================================================
;;; Extraction Analysis
;;; ============================================================

(doc 'section 'analysis)

;;; extraction-report : ExtractionState × ClassId → String
;;; Generate a report showing the extraction choice for a class.
(define (extraction-report state class-id)
  (doc 'type (-> ExtractionState ClassId String))
  (doc 'description "Generate extraction report for a class.")
  (doc 'export #t)
  (let* ([eg (state-egraph state)]
         [root (egraph-find eg class-id)]
         [costs (state-costs state)]
         [best-nodes (state-best-nodes state)]
         [best (hashtable-ref best-nodes root #f)]
         [all-nodes (eclass-get-nodes (egraph-classes eg) root)])
    (with-output-to-string
      (lambda ()
        (printf "E-class ~a:~n" root)
        (printf "  Selected: ~a (cost=~a)~n"
                (if best (enode->string best) "none")
                (class-cost costs root))
        (printf "  Alternatives (~a total):~n" (length all-nodes))
        (for-each
         (lambda (node)
           (unless (eq? node best)
             (printf "    - ~a~n" (enode->string node))))
         all-nodes)))))

;;; compare-extractions : ExtractionState × ClassId × (List CostModel) → (List (CostModel × Term × Cost))
;;; Compare what different cost models would extract for a class.
(define (compare-extractions eg class-id cost-models)
  (doc 'type (-> EGraph ClassId (List CostModel) (List (Triple CostModel Term Nat))))
  (doc 'description "Compare extractions across different cost models.")
  (doc 'export #t)
  (map (lambda (cm)
         (let* ([state (make-extraction-state eg cm)]
                [term (extract state class-id)]
                [cost (class-cost (state-costs state) (egraph-find eg class-id))])
           (list cm term cost)))
       cost-models))

;;; ============================================================
;;; Debugging
;;; ============================================================

(doc 'section 'debug)

(define (extraction-debug state class-id)
  (doc 'type (-> ExtractionState ClassId Void))
  (doc 'description "Print extraction debug info.")
  (doc 'export #t)
  (display (extraction-report state class-id)))

(define (optimize-debug term rules cost-model)
  (doc 'type (-> Term (List Rule) CostModel Term))
  (doc 'description "Optimize with debug output.")
  (doc 'export #t)
  (let ([eg (make-egraph)])
    (printf "Input term: ~a~n" term)
    (let ([root (egraph-add-term! eg term)])
      (printf "Initial e-graph: ~a classes, ~a nodes~n"
              (egraph-class-count eg) (egraph-node-count eg))
      (let ([result (saturate-simple eg rules)])
        (printf "After saturation: ~a classes, ~a nodes (~a rules applied)~n"
                (result-final-classes result)
                (result-final-nodes result)
                (result-rules-applied result))
        (let ([state (make-extraction-state eg cost-model)])
          (printf "~n~a~n" (extraction-report state root))
          (let ([extracted (extract state root)])
            (printf "Extracted: ~a~n" extracted)
            extracted))))))

