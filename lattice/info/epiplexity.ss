;;; @module epiplexity
;;; @requires prelude entropy
;;; @description Prequential and requential epiplexity measures for sequence prediction evaluation and model comparison.
;;; @purity total
;;; @stability stable

(require 'prelude)
(require 'entropy)

(doc 'module 'epiplexity)
(doc 'description "Epiplexity measures: structural information extraction from data")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'prequential-epiplexity)

(define (prequential-epiplexity online-losses final-losses)
  (doc 'export #t)
  (doc 'type '(-> (List Number) (List Number) Number))
  (doc 'description "Estimate epiplexity from online and final loss sequences")
  (cond
   [(and (null? online-losses) (null? final-losses)) 0]
   [(not (= (length online-losses) (length final-losses)))
    (error 'prequential-epiplexity "loss lists must have equal length")]
   [else
    (fold-left + 0
               (map - online-losses final-losses))]))

(define (prequential-epiplexity-scalar online-losses baseline-loss)
  (doc 'export #t)
  (doc 'type '(-> (List Number) Number Number))
  (doc 'description "Estimate epiplexity assuming constant final baseline loss")
  (if (null? online-losses)
      0
      (fold-left + 0
                 (map (lambda (l) (- l baseline-loss))
                      online-losses))))

(doc 'section 'requential-epiplexity)

(define (requential-epiplexity teacher-dists student-dists)
  (doc 'export #t)
  (doc 'type '(-> (List (List Number)) (List (List Number)) Number))
  (doc 'description "Estimate epiplexity using Requential Coding (Teacher-Student approach)")
  (cond
   [(and (null? teacher-dists) (null? student-dists)) 0]
   [(not (= (length teacher-dists) (length student-dists)))
    (error 'requential-epiplexity "distribution lists must have equal length")]
   [else
    (fold-left + 0
               (map kl-divergence teacher-dists student-dists))]))
