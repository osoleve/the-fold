;;; playpen/quill/education.ss — Quill education helpers
;;;
;;; Exercises are pure, data-first objects that can be graded against
;;; a run + answer. Progress is tracked in state meta.

;;; ============================================================
;;; Records
;;; ============================================================

(define-record-type quill-check%
  (fields id description predicate meta))

(define make-quill-check make-quill-check%)
(define quill-check-id quill-check%-id)
(define quill-check-description quill-check%-description)
(define quill-check-predicate quill-check%-predicate)
(define quill-check-meta quill-check%-meta)

(define-record-type quill-exercise%
  (fields id title prompt checks on-pass meta))

(define make-quill-exercise make-quill-exercise%)
(define quill-exercise-id quill-exercise%-id)
(define quill-exercise-title quill-exercise%-title)
(define quill-exercise-prompt quill-exercise%-prompt)
(define quill-exercise-checks quill-exercise%-checks)
(define quill-exercise-on-pass quill-exercise%-on-pass)
(define quill-exercise-meta quill-exercise%-meta)

;;; ============================================================
;;; Utilities
;;; ============================================================

(define (quill-education-alist-ref alist key default)
  (let ([p (assq key alist)])
       (if p (cdr p) default)))

(define (quill-education-eval-predicate pred story run answer)
  (cond
   [(procedure? pred) (and (pred story run answer) #t)]
   [(boolean? pred) pred]
   [else #f]))

(define (quill-check-result check ok?)
  `((id . ,(quill-check-id check))
    (description . ,(quill-check-description check))
    (pass? . ,ok?)))

;;; ============================================================
;;; Story helpers
;;; ============================================================

(define (quill-story-exercises story)
  (let ([p (assq 'exercises (quill-story-meta story))])
       (if p (cdr p) '())))

(define (quill-story-exercise story exercise-id)
  (let loop ([xs (quill-story-exercises story)])
       (cond
        [(null? xs) #f]
        [(eq? (quill-exercise-id (car xs)) exercise-id) (car xs)]
        [else (loop (cdr xs))])))

;;; ============================================================
;;; Progress tracking
;;; ============================================================

(define (quill-run-education-statuses run)
  (quill-state-meta-get (quill-run-state run) 'education '()))

(define (quill-education-status run exercise-id)
  (let* ([xs (quill-run-education-statuses run)]
         [p (assq exercise-id xs)])
        (if p (cdr p) 'unseen)))

(define (quill-education-set-status run exercise-id status)
  (let* ([xs (quill-run-education-statuses run)]
         [p (assq exercise-id xs)]
         [xs2 (if p
                  (cons (cons exercise-id status) (remq p xs))
                  (cons (cons exercise-id status) xs))]
         [state1 (quill-state-meta-set (quill-run-state run) 'education xs2)])
        (quill-run-with-state run state1)))

(define (quill-run-education-results run)
  (quill-state-meta-get (quill-run-state run) 'education-results '()))

(define (quill-education-record-result run exercise-id result)
  (let* ([xs (quill-run-education-results run)]
         [entry (cons exercise-id result)]
         [state1 (quill-state-meta-set (quill-run-state run)
                                       'education-results
                                       (cons entry xs))])
        (quill-run-with-state run state1)))

;;; ============================================================
;;; Grading
;;; ============================================================

(define (quill-grade-exercise story run exercise answer)
  (let* ([checks (quill-exercise-checks exercise)]
         [results
          (map (lambda (check)
                       (let ([ok? (quill-education-eval-predicate
                                   (quill-check-predicate check) story run answer)])
                            (quill-check-result check ok?)))
               checks)]
         [failed (filter (lambda (r) (not (quill-education-alist-ref r 'pass? #f))) results)]
         [pass? (null? failed)])
        `((exercise . ,(quill-exercise-id exercise))
          (pass? . ,pass?)
          (failed . ,(map (lambda (r) (quill-education-alist-ref r 'id 'missing)) failed))
          (checks . ,results))))

(define (quill-apply-exercise story run exercise answer apply-effects)
  (let* ([result (quill-grade-exercise story run exercise answer)]
         [pass? (quill-education-alist-ref result 'pass? #f)]
         [status (if pass? 'passed 'failed)]
         [run1 (quill-education-set-status run (quill-exercise-id exercise) status)]
         [run2 (quill-education-record-result run1 (quill-exercise-id exercise) result)]
         [run3 (if pass?
                   (apply-effects story run2 (or (quill-exercise-on-pass exercise) '()))
                   run2)])
        (values run3 result)))

(define (quill-exercise-prompt->string story run exercise)
  (let ([prompt (quill-exercise-prompt exercise)])
       (cond
        [(string? prompt) prompt]
        [(procedure? prompt) (prompt story run)]
        [else (format "~a" prompt)])))
