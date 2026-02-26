;;; user/sft/kg/coverage.ss — KG concept coverage analysis
;;;
;;; Analyzes which KG concepts are well-represented in SFT data and which
;;; have gaps. Drives targeted generation for under-represented concepts.
;;;
;;; Usage:
;;;   (load "user/sft/kg/coverage.ss")
;;;   (define report (coverage/analyze "data/phase1-combined/all.jsonl"))
;;;   (coverage/print-report report)
;;;   (coverage/gaps report)          ; Concepts with 0 samples
;;;   (coverage/thin report 5)        ; Concepts with < 5 samples

(unless (top-level-bound? 'walk-all-manifests)
  (load "user/sft/extract/walker.ss"))
(unless (top-level-bound? 'make-sample)
  (load "user/sft/extract/emit.ss"))
(load "boundary/io/json.ss")

;;; ====
;;; JSONL reading (reuse from rewrite.ss pattern)
;;; ====

(define (coverage/read-jsonl path)
  (let ([p (open-input-file path)])
    (let loop ([acc '()])
      (let ([line (get-line p)])
        (if (eof-object? line)
            (begin (close-input-port p) (reverse acc))
            (let ([parsed (guard (ex [else #f])
                            (parse-json-string line))])
              (loop (if parsed (cons parsed acc) acc))))))))

;;; ====
;;; Build skill→concepts and module→skill maps from walker IR
;;; ====

(define (coverage/build-file-skill-map ir-records)
  ;; Returns alist: (("lattice/foo/bar.ss" . skill-name) ...)
  ;; Maps source file paths to skill names.
  ;; JSONL source_module field contains file paths, not module names.
  (let loop ([remaining ir-records] [acc '()])
    (if (null? remaining)
        acc
        (let* ([ir (car remaining)]
               [file (cdr (assq 'file ir))]
               [skill (cdr (assq 'skill ir))])
          (if (assoc file acc)  ;; string keys → use assoc, not assq
              (loop (cdr remaining) acc)
              (loop (cdr remaining) (cons (cons file skill) acc)))))))

(define (coverage/build-skill-concepts-map)
  ;; Returns alist: ((skill-name concept1 concept2 ...) ...)
  ;; Requires KG to be initialized.
  (map (lambda (skill)
         (cons skill (kg-skill-concepts skill)))
       (kg-skills)))

;;; ====
;;; Sample → concept mapping
;;; ====

(define (coverage/sample-concepts sample file-skill-map skill-concepts-map)
  ;; Given a sample, return list of concepts it covers.
  ;; Sample has source_module field (a file path string) → look up skill → look up concepts.
  (let* ([mod-entry (assq 'source_module sample)]
         [file-path (and mod-entry (cdr mod-entry))])
    (if (not (string? file-path))
        '()
        (let ([skill-entry (assoc file-path file-skill-map)])  ;; string keys → assoc
          (if (not skill-entry)
              '()
              (let* ([skill (cdr skill-entry)]
                     [concepts-entry (assq skill skill-concepts-map)])
                (if concepts-entry
                    (cdr concepts-entry)
                    '())))))))

;;; ====
;;; Coverage analysis
;;; ====

(define (coverage/analyze jsonl-path)
  ;; Analyze concept coverage from a JSONL file.
  ;; Returns: ((concepts . all-concepts)
  ;;           (counts . ((concept . count) ...))
  ;;           (total-samples . N)
  ;;           (skills . skill-concepts-map))
  (printf "Loading samples from ~a...\n" jsonl-path)
  (let* ([samples (coverage/read-jsonl jsonl-path)]
         [_ (printf "  ~a samples loaded\n" (length samples))]

         ;; Build walker IR for file→skill mapping
         [_ (printf "Walking manifests for file→skill map...\n")]
         [ir (walk-all-manifests)]
         [file-skill-map (coverage/build-file-skill-map ir)]
         [_ (printf "  ~a unique file→skill mappings\n" (length file-skill-map))]

         ;; Initialize KG for concept data
         [_ (printf "Initializing KG...\n")]
         [_ (load "lattice/meta/meta.ss")]
         [_ (lattice-init!)]
         [skill-concepts-map (coverage/build-skill-concepts-map)]
         [all-concepts (kg-concepts)]
         [_ (printf "  ~a concepts in KG\n" (length all-concepts))]

         ;; Count samples per concept
         [counts
          (let loop ([remaining samples]
                     [concept-counts (map (lambda (c) (cons c 0)) all-concepts)])
            (if (null? remaining)
                concept-counts
                (let* ([sample (car remaining)]
                       [concepts (coverage/sample-concepts sample
                                   file-skill-map skill-concepts-map)])
                  (for-each
                    (lambda (concept)
                      (let ([entry (assq concept concept-counts)])
                        (when entry
                          (set-cdr! entry (+ (cdr entry) 1)))))
                    concepts)
                  (loop (cdr remaining) concept-counts))))])

    `((concepts . ,all-concepts)
      (counts . ,(list-sort (lambda (a b) (> (cdr a) (cdr b))) counts))
      (total-samples . ,(length samples))
      (skills . ,skill-concepts-map))))

;;; ====
;;; Report accessors
;;; ====

(define (coverage/gaps report)
  ;; Concepts with 0 samples.
  (filter (lambda (p) (= (cdr p) 0))
          (cdr (assq 'counts report))))

(define (coverage/thin report min-count)
  ;; Concepts with < min-count samples.
  (filter (lambda (p) (< (cdr p) min-count))
          (cdr (assq 'counts report))))

(define (coverage/well-covered report min-count)
  ;; Concepts with >= min-count samples.
  (filter (lambda (p) (>= (cdr p) min-count))
          (cdr (assq 'counts report))))

(define (coverage/skills-for-concept report concept-name)
  ;; Which skills provide a given concept?
  (filter-map
    (lambda (entry)
      (if (memq concept-name (cdr entry))
          (car entry)
          #f))
    (cdr (assq 'skills report))))

(define (coverage/concept-distribution report)
  ;; Histogram: count of concepts by sample-count bucket.
  (let* ([counts (cdr (assq 'counts report))]
         [buckets '((0 . 0) (1-5 . 0) (6-20 . 0) (21-50 . 0) (51-100 . 0) (100+ . 0))])
    (for-each
      (lambda (p)
        (let* ([n (cdr p)]
               [bucket (cond
                         [(= n 0) '0]
                         [(<= n 5) '1-5]
                         [(<= n 20) '6-20]
                         [(<= n 50) '21-50]
                         [(<= n 100) '51-100]
                         [else '100+])]
               [entry (assq bucket buckets)])
          (when entry
            (set-cdr! entry (+ (cdr entry) 1)))))
      counts)
    buckets))

;;; ====
;;; Pretty-print report
;;; ====

(define (coverage/print-report report)
  (let* ([counts (cdr (assq 'counts report))]
         [total (cdr (assq 'total-samples report))]
         [gaps (coverage/gaps report)]
         [thin5 (coverage/thin report 5)]
         [dist (coverage/concept-distribution report)])

    (printf "\n=== KG Concept Coverage Report ===\n")
    (printf "Total samples: ~a\n" total)
    (printf "Total concepts: ~a\n" (length counts))
    (printf "Zero-coverage: ~a concepts\n" (length gaps))
    (printf "Thin (< 5): ~a concepts\n" (length thin5))

    (printf "\nDistribution:\n")
    (for-each (lambda (p) (printf "  ~a samples: ~a concepts\n" (car p) (cdr p)))
              dist)

    (when (pair? gaps)
      (printf "\nZero-coverage concepts:\n")
      (for-each (lambda (p) (printf "  ~a\n" (car p)))
                (if (> (length gaps) 20) (list-head gaps 20) gaps))
      (when (> (length gaps) 20)
        (printf "  ... (~a more)\n" (- (length gaps) 20))))

    (printf "\nTop-10 most covered:\n")
    (for-each (lambda (p) (printf "  ~a: ~a samples\n" (car p) (cdr p)))
              (if (> (length counts) 10) (list-head counts 10) counts))))

;;; ====
;;; Generate targeted samples for low-coverage concepts
;;; ====

(define (coverage/targets report max-targets min-skill-exports)
  ;; Select concepts that need more samples, paired with skills that provide them.
  ;; Returns: ((concept skill functions...) ...)
  ;; where functions are walker IR records for that skill.
  (let* ([thin (coverage/thin report 5)]
         [skill-concepts (cdr (assq 'skills report))]
         ;; For each thin concept, find skills and their functions
         [targets
          (filter-map
            (lambda (thin-entry)
              (let* ([concept (car thin-entry)]
                     [current-count (cdr thin-entry)]
                     [skills (coverage/skills-for-concept report concept)])
                (if (null? skills)
                    #f  ; no skill provides this concept
                    `((concept . ,concept)
                      (current-count . ,current-count)
                      (skills . ,skills)))))
            thin)])
    ;; Sort by current count ascending (most underrepresented first)
    (let ([sorted (list-sort (lambda (a b)
                               (< (cdr (assq 'current-count a))
                                  (cdr (assq 'current-count b))))
                             targets)])
      (if (> (length sorted) max-targets)
          (list-head sorted max-targets)
          sorted))))

;;; ====
;;; Load
;;; ====

(printf "coverage.ss loaded.\n")
(printf "  (coverage/analyze \"path/to/all.jsonl\")  - Full coverage analysis\n")
(printf "  (coverage/print-report report)            - Pretty-print results\n")
(printf "  (coverage/gaps report)                    - Zero-coverage concepts\n")
(printf "  (coverage/thin report N)                  - Concepts with < N samples\n")
(printf "  (coverage/targets report max min-exports) - Generate target list\n")
