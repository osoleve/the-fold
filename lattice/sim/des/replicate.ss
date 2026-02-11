(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
;;; @module des/replicate
;;; @requires prelude des/engine des/world prng summary-stats
(require 'prelude)
(require 'des/engine)
(require 'des/world)
(require 'prng)
(require 'summary-stats)

(doc 'module 'des/replicate)
(doc 'description "Discrete Event Simulation — replication runner. Run N independent replications of a simulation with split RNGs, collect results, and compute summary statistics. Each replication gets a deterministic but independent RNG stream.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;;---------------------------------------------------------------------------
;;; Replication
;;;---------------------------------------------------------------------------

(doc 'section 'replication)

(define (des-replicate config make-world n seed)
  (doc 'type '(-> DESConfig (-> RNG DESWorld) Nat Nat (List DESWorld)))
  (doc 'description "Run n independent replications. make-world takes an RNG and returns the initial world for that replication. Each replication gets a unique PCG stream seeded from seed. Returns list of final world states.")
  (map (lambda (i)
         (let* ([rng (make-pcg seed (+ i 1))]
                [w0 (make-world rng)])
           (des-run-final config w0)))
       (iota n)))

(define (des-replicate-metric config make-world n seed metric-name)
  (doc 'type '(-> DESConfig (-> RNG DESWorld) Nat Nat Symbol (List Number)))
  (doc 'description "Run n replications and extract a named metric from each final state.")
  (map (lambda (w) (world-metric w metric-name))
       (des-replicate config make-world n seed)))

(define (des-replicate-summary config make-world n seed metric-name)
  (doc 'type '(-> DESConfig (-> RNG DESWorld) Nat Nat Symbol Alist))
  (doc 'description "Run n replications, extract metric, compute summary statistics. Returns alist with mean, std-dev, min, max, median, sem.")
  (let ([values (des-replicate-metric config make-world n seed metric-name)])
    (if (null? values)
        '((mean . 0) (std-dev . 0) (min . 0) (max . 0) (median . 0) (sem . 0) (n . 0))
        (list (cons 'mean (mean values))
              (cons 'std-dev (if (< (length values) 2) 0 (std-dev values)))
              (cons 'min (apply min values))
              (cons 'max (apply max values))
              (cons 'median (median values))
              (cons 'sem (if (< (length values) 2) 0 (sem values)))
              (cons 'n (length values))))))

;;;---------------------------------------------------------------------------
;;; Multi-metric collection
;;;---------------------------------------------------------------------------

(doc 'section 'multi-metric)

(define (des-replicate-metrics config make-world n seed metric-names)
  (doc 'type '(-> DESConfig (-> RNG DESWorld) Nat Nat (List Symbol) Alist))
  (doc 'description "Run n replications, compute summary statistics for each named metric. Returns alist of (metric-name . summary-alist).")
  (let ([finals (des-replicate config make-world n seed)])
    (map (lambda (name)
           (let ([values (map (lambda (w) (world-metric w name)) finals)])
             (cons name
                   (if (null? values)
                       '((mean . 0) (n . 0))
                       (list (cons 'mean (mean values))
                             (cons 'std-dev (if (< (length values) 2) 0 (std-dev values)))
                             (cons 'min (apply min values))
                             (cons 'max (apply max values))
                             (cons 'n (length values)))))))
         metric-names)))
