;;; core/info-theory/epiplexity.ss — Epiplexity and Complexity Measures
;;;
;;; Implementation of Epiplexity measures from "From Entropy to Epiplexity" (2026).
;;; Epiplexity measures the structural information a computationally-bounded
;;; observer can extract from data, separating it from random noise (entropy).
;;;
;;; Defines:
;;;   - prequential-epiplexity: Estimation via loss curves (Prequential Coding).
;;;   - requential-epiplexity: Estimation via KL divergence sum (Requential Coding).
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - entropy.ss
;;;   - statistical-measures.ss

(load "core/base/prelude.ss")
(load "lattice/info/entropy.ss")
(load "lattice/info/statistical-measures.ss")

;;; ====
;;; Prequential Epiplexity
;;; ====

;;; prequential-epiplexity : (List Number) × (List Number) → Number
;;; Estimate epiplexity from online and final loss sequences.
;;; Epiplexity ≈ TotalOnlineLoss - TotalFinalLoss
;;;
;;; K(X) ≈ Σ (L_online(t) - L_final(t))
;;;
;;; inputs:
;;;   online-losses: List of losses encountered during training L(P_t, x_t).
;;;   final-losses: List of losses of the final model on the same data L(P_T, x_t).
;;;                 Must be same length as online-losses.
;;; errors:
;;;   Raises error if lists have different lengths.
(define (prequential-epiplexity online-losses final-losses)
  (cond
   [(and (null? online-losses) (null? final-losses)) 0]
   [(not (= (length online-losses) (length final-losses)))
    (error 'prequential-epiplexity "loss lists must have equal length")]
   [else
    (fold-left + 0
               (map - online-losses final-losses))]))

;;; prequential-epiplexity-scalar : (List Number) × Number → Number
;;; Estimate epiplexity assuming a constant final baseline loss (entropy floor).
;;; Useful when full final loss trajectory is unavailable.
(define (prequential-epiplexity-scalar online-losses baseline-loss)
  (if (null? online-losses)
      0
      (fold-left + 0
                 (map (lambda (l) (- l baseline-loss))
                      online-losses))))

;;; ====
;;; Requential Epiplexity
;;; ====

;;; requential-epiplexity : (List (List Number)) × (List (List Number)) → Number
;;; Estimate epiplexity using Requential Coding (Teacher-Student approach).
;;;
;;; Epiplexity ≈ Σ KL(P_teacher(t) || P_student(t))
;;;
;;; inputs:
;;;   teacher-dists: List of probability distributions (lists) from the teacher.
;;;   student-dists: List of probability distributions (lists) from the student.
;;;                  Must be same length.
;;; errors:
;;;   Raises error if distribution lists have different lengths.
(define (requential-epiplexity teacher-dists student-dists)
  (cond
   [(and (null? teacher-dists) (null? student-dists)) 0]
   [(not (= (length teacher-dists) (length student-dists)))
    (error 'requential-epiplexity "distribution lists must have equal length")]
   [else
    (fold-left + 0
               (map kl-divergence teacher-dists student-dists))]))
