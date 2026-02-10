;;; lattice/dataset/test-dataset.ss — Tests for Dataset SDK
;;; Run with: scheme --script lattice/dataset/test-dataset.ss

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/dataset/sample.ss")
(load "lattice/dataset/parameter.ss")
(load "lattice/dataset/presentation.ss")
(load "lattice/dataset/distractor.ss")
(load "lattice/dataset/difficulty.ss")
(load "lattice/dataset/format/sexp.ss")
(load "lattice/dataset/format/jsonl.ss")
(load "lattice/random/prng.ss")

;;; ============================================================
;;; Helpers
;;; ============================================================

;;; string-contains? : String × String → Boolean
(define (string-contains? str substr)
  (let ([len1 (string-length str)]
        [len2 (string-length substr)])
    (let loop ([i 0])
      (cond
        [(> (+ i len2) len1) #f]
        [(string=? (substring str i (+ i len2)) substr) #t]
        [else (loop (+ i 1))]))))

;;; unique-strings : (List String) → (List String)
;;; Return only unique strings from list
(define (unique-strings strings)
  (let loop ([remaining strings] [seen '()])
    (if (null? remaining)
        (reverse seen)
        (if (member (car remaining) seen)
            (loop (cdr remaining) seen)
            (loop (cdr remaining) (cons (car remaining) seen))))))

;;; ============================================================
;;; Sample Tests
;;; ============================================================

(test-group "sample"
  (define-test "make-sample creates valid sample"
    (let ([s (make-sample "test-001"
                          '("frame1" "frame2")
                          "What is the answer?"
                          '((A . "Option A") (B . "Option B")
                            (C . "Option C") (D . "Option D"))
                          'B
                          '((category . spatial) (difficulty . 1)))])
      (assert-true (sample? s))
      (assert-equal "test-001" (sample-id s))
      (assert-equal 2 (length (sample-frames s)))
      (assert-equal "What is the answer?" (sample-question s))
      (assert-equal 'B (sample-answer s))))

  (define-test "sample lenses work"
    (let* ([s (make-sample "id1" '("f1") "Q?" '((A . "a")) 'A '())]
           [s2 (set-lens sample-id-lens "id2" s)])
      (assert-equal "id2" (sample-id s2))
      (assert-equal "id1" (view sample-id-lens s))))

  (define-test "sample->prompt generates correct format"
    (let* ([s (make-sample "test"
                           '("[frame]")
                           "Which is larger?"
                           '((A . "Left") (B . "Right")
                             (C . "Same") (D . "Unknown"))
                           'A '())]
           [prompt (sample->prompt s)])
      (assert-true (string? prompt))
      (assert-true (> (string-length prompt) 0))))

  (define-test "make-options creates 4 options"
    (let ([opts (make-options "First" "Second" "Third" "Fourth")])
      (assert-equal 4 (length opts))
      (assert-equal "First" (option-by-label opts 'A)))))

;;; ============================================================
;;; Parameter Tests
;;; ============================================================

(test-group "parameter"
  (define-test "range creates valid param range"
    (let ([r (uniform-range 'mass 1.0 10.0)])
      (assert-true (param-range? r))
      (assert-equal 'mass (param-range-name r))
      (assert-equal 1.0 (param-range-min r))
      (assert-equal 10.0 (param-range-max r))))

  (define-test "param-sample returns value in range"
    (let* ([r (uniform-range 'x 0.0 100.0)]
           [rng (make-prng 42)])
      (do ([i 0 (+ i 1)])
          ((>= i 20))
        (let ([val (param-sample-default r rng)])
          (assert-true (>= val 0.0))
          (assert-true (<= val 100.0))))))

  (define-test "range-int returns integers"
    (let* ([r (range-int 'count 1 10)]
           [rng (make-prng 123)])
      (do ([i 0 (+ i 1)])
          ((>= i 10))
        (let ([val (param-sample-default r rng)])
          (assert-true (integer? val))
          (assert-true (>= val 1))
          (assert-true (<= val 10))))))

  (define-test "range-choice samples from choices"
    (let* ([r (range-choice 'color '(red green blue))]
           [rng (make-prng 789)])
      (do ([i 0 (+ i 1)])
          ((>= i 10))
        (let ([val (param-sample-default r rng)])
          (assert-true (if (memq val '(red green blue)) #t #f))))))

  (define-test "param-set-sample returns all params"
    (let* ([ps (make-param-set
                (list (uniform-range 'm1 1.0 5.0)
                      (uniform-range 'm2 1.0 5.0)
                      (range-int 'n 1 10))
                (lambda (_) #t))]
           [rng (make-prng 456)]
           [params (param-set-sample ps rng 100)])
      (assert-equal 3 (length params))
      (assert-true (number? (param-get params 'm1)))
      (assert-true (number? (param-get params 'm2)))
      (assert-true (integer? (param-get params 'n)))))

  (define-test "interpolate-text replaces placeholders"
    (let* ([template "Ball A has mass ${m1} kg and velocity ${v1} m/s."]
           [params '((m1 . 2) (v1 . 3.5))]
           [result (interpolate-text template params)])
      (assert-true (string? result))
      (assert-true (not (string-contains? result "${")))
      (assert-true (string-contains? result "2"))))

  (define-test "param-interpolate works for numeric"
    (let* ([r (uniform-range 'x 0.0 10.0)]
           [v1 2.0]
           [v2 8.0]
           [mid (param-interpolate r v1 v2 0.5)])
      (assert-true (< (abs (- mid 5.0)) 0.01)))))

;;; ============================================================
;;; Presentation Tests
;;; ============================================================

(test-group "presentation"
  (define-test "text-block has correct dimensions"
    (let ([tb (text "Hello world, this is a test." 20)])
      (assert-true (text-block? tb))
      (assert-equal 20 (element-width tb))))

  (define-test "vstack combines elements vertically"
    (let* ([t1 (text "Line 1" 10)]
           [t2 (text "Line 2" 10)]
           [layout (layout-vstack (list t1 t2))])
      (assert-true (layout-vstack? layout))
      (assert-equal 2 (element-height layout))))

  (define-test "render-layout produces string"
    (let* ([layout (layout-vstack
                    (list (text "Title" 20)
                          (gap 1)
                          (text "Content here" 20)))]
           [result (render-layout layout)])
      (assert-true (string? result))
      (assert-true (> (string-length result) 0))))

  (define-test "given-box renders parameters"
    (let* ([box (given-box '((m1 . 2) (v1 . 5)) 15)]
           [layout (layout-vstack (list box))]
           [result (render-layout layout)])
      (assert-true (string-contains? result "Given")))))

;;; ============================================================
;;; Distractor Tests
;;; ============================================================

(test-group "distractor"
  (define-test "distractor-nearby perturbs value"
    (let* ([strategy (distractor-nearby 0.1)]
           [rng (make-prng 42)]
           [val (apply-distractor strategy 10.0 rng)])
      (assert-true (number? val))
      (assert-true (not (= val 10.0)))))

  (define-test "distractor-sign-flip negates"
    (let* ([rng (make-prng 42)]
           [val (apply-distractor distractor-sign-flip 5.0 rng)])
      (assert-equal -5.0 val)))

  (define-test "generate-distractors creates distinct values"
    (let* ([strategies numeric-distractors]
           [rng (make-prng 42)]
           [distractors (generate-distractors 10.0 strategies 3 rng)])
      (assert-equal 3 (length distractors))
      ;; All should be different from correct
      (assert-true (andmap (lambda (d) (not (= d 10.0))) distractors))
      ;; Should be distinct from each other (approximately)
      (assert-true (not (approx-equal? (car distractors) (cadr distractors))))))

  (define-test "comparison-distractors work for symbols"
    (let* ([rng (make-prng 42)]
           [distractors (generate-distractors 'A comparison-distractors 3 rng)])
      (assert-equal 3 (length distractors))
      (assert-true (andmap (lambda (d) (not (eq? d 'A))) distractors))))

  (define-test "counting options are unique after integer formatting"
    ;; Regression test: counting distractors were creating duplicates
    ;; like A) 2, B) 2, C) 2, D) 3 due to rounding collisions
    (let* ([formatter (lambda (n) (number->string (exact (round n))))]
           [rng (make-prng 42)])
      ;; Test several small counting values (where collisions are likely)
      (let loop ([i 0] [correct 2])
        (when (< i 10)
          (let* ([result (make-numeric-options correct counting-distractors rng formatter)]
                 [options (car result)]
                 [strings (map cdr options)])
            ;; All 4 options should be unique strings
            (assert-equal 4 (length (unique-strings strings))))
          (loop (+ i 1) (+ correct 1)))))))

;;; ============================================================
;;; Difficulty Tests
;;; ============================================================

(test-group "difficulty"
  (define-test "difficulty-vector construction"
    (let ([dv (make-difficulty-vector 2 3 5 2 1)])
      (assert-true (difficulty-vec? dv))
      (assert-equal 2 (dv-inference-depth dv))
      (assert-equal 3 (dv-object-count dv))))

  (define-test "difficulty-score computes weighted sum"
    (let* ([dv (make-difficulty-vector 0 2 1 1 0)]
           [score (difficulty-score-default dv)])
      (assert-true (number? score))
      (assert-true (>= score 0))))

  (define-test "difficulty-tier classifies correctly"
    (assert-equal 'trivial (difficulty-tier 1.0))
    (assert-equal 'easy (difficulty-tier 3.0))
    (assert-equal 'medium (difficulty-tier 5.0))
    (assert-equal 'hard (difficulty-tier 7.0))
    (assert-equal 'expert (difficulty-tier 9.0)))

  (define-test "spatial-difficulty is easy"
    (let ([score (difficulty-score-default spatial-difficulty)])
      (assert-true (< score 4.0)))))

;;; ============================================================
;;; Format Tests
;;; ============================================================

(test-group "format-sexp"
  (define-test "sample roundtrip through sexp"
    (let* ([s (make-sample "test-001"
                           '("frame1" "frame2")
                           "What is X?"
                           '((A . "1") (B . "2") (C . "3") (D . "4"))
                           'C
                           '((category . test)))]
           [sexp (sample->canonical-sexp s)]
           [s2 (canonical-sexp->sample sexp)])
      (assert-equal (sample-id s) (sample-id s2))
      (assert-equal (sample-question s) (sample-question s2))
      (assert-equal (sample-answer s) (sample-answer s2)))))

(test-group "format-jsonl"
  (define-test "sample->json creates valid structure"
    (let* ([s (make-sample "json-001"
                           '("frame")
                           "Question?"
                           '((A . "a") (B . "b") (C . "c") (D . "d"))
                           'A '())]
           [json (sample->json s)])
      (assert-true (json-object? json))))

  (define-test "json roundtrip preserves data"
    (let* ([s (make-sample "rt-001"
                           '("f1" "f2")
                           "Q?"
                           '((A . "x") (B . "y") (C . "z") (D . "w"))
                           'B
                           '((cat . test)))]
           [json (sample->json s)]
           [s2 (json->sample json)])
      (assert-equal (sample-id s) (sample-id s2))
      (assert-equal (sample-answer s) (sample-answer s2))))

  (define-test "json->string produces valid output"
    (let* ([s (make-sample "str-001"
                           '("frame")
                           "Q?"
                           '((A . "a") (B . "b") (C . "c") (D . "d"))
                           'A '())]
           [json (sample->json s)]
           [str (json->string json)])
      (assert-true (string? str))
      (assert-true (> (string-length str) 10))
      ;; Should start with { and end with }
      (assert-true (char=? (string-ref str 0) #\{)))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests-and-exit)
