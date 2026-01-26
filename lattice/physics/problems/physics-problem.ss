;;; lattice/physics/problems/physics-problem.ss — Physics Problem DSL
;;; @module physics-problem
;;; @requires prelude dataset simulation

(load "core/base/prelude.ss")
(load "lattice/linalg/vec2.ss")
(load "lattice/dataset/sample.ss")
(load "lattice/dataset/parameter.ss")
(load "lattice/dataset/presentation.ss")
(load "lattice/dataset/distractor.ss")
(load "lattice/dataset/difficulty.ss")
(load "lattice/physics/problems/simulation.ss")

(doc 'module 'physics-problem)
(doc 'description "Physics problem DSL for defining and generating visual reasoning problems. Composes the Dataset SDK with classical physics simulation.")
(doc 'layer 'lattice)
(doc 'purity 'partial)

;;; ============================================================
;;; Part 1: Problem Template Definition
;;; ============================================================
;;;
;;; A physics problem template specifies:
;;;   - Parameter ranges (what can vary)
;;;   - World setup (how to create entities from params)
;;;   - Question generation (what to ask)
;;;   - Answer computation (ground truth from simulation)
;;;   - Presentation variants (how to display)

(doc 'section 'template)

;;; make-physics-problem : Symbol × ... → PhysicsProblem
;;; Create a physics problem template
(define (make-physics-problem name category params world-setup question-gen answer-compute difficulty-vec)
  (doc 'export #t)
  (list 'physics-problem
        name
        category
        params          ; ParamSet
        world-setup     ; Alist → World
        question-gen    ; Alist → String
        answer-compute  ; World × World × Alist → Any
        difficulty-vec))

(define (physics-problem? x)
  (and (pair? x) (eq? (car x) 'physics-problem)))

(define (problem-name p) (list-ref p 1))
(define (problem-category p) (list-ref p 2))
(define (problem-params p) (list-ref p 3))
(define (problem-world-setup p) (list-ref p 4))
(define (problem-question-gen p) (list-ref p 5))
(define (problem-answer-compute p) (list-ref p 6))
(define (problem-difficulty p) (list-ref p 7))

;;; ============================================================
;;; Part 2: Problem Instance Generation
;;; ============================================================

(doc 'section 'generation)

;;; generate-problem-instance : PhysicsProblem × RNG × ProblemConfig → Sample
;;; Generate a single problem instance from a template
(define (generate-problem-instance problem rng config)
  (doc 'export #t)
  (let* ([params (param-set-sample (problem-params problem) rng 100)]
         [initial-world ((problem-world-setup problem) params)]
         [n-frames 8]  ; default number of frames
         [sim-result (capture-frames-with-final initial-world config n-frames)]
         [frames (car sim-result)]
         [final-world (cdr sim-result)]
         [question-text ((problem-question-gen problem) params)]
         [correct-answer ((problem-answer-compute problem) initial-world final-world params)]
         [options (generate-problem-options correct-answer (problem-category problem) rng)]
         [correct-label (find-correct-label options correct-answer)]
         [id (generate-problem-id (problem-name problem) rng)]
         [metadata `((category . ,(problem-category problem))
                     (difficulty . ,(difficulty-score-default (problem-difficulty problem)))
                     (params . ,params))])
    (make-sample id frames question-text options correct-label metadata)))

;;; generate-problem-id : Symbol × RNG → String
;;; Generate unique problem ID
(define (generate-problem-id name rng)
  (string-append (symbol->string name)
                 "-"
                 (number->string (prng-next-int rng 100000))))

;;; ============================================================
;;; Part 3: Option Generation
;;; ============================================================

(doc 'section 'options)

;;; generate-problem-options : Any × Symbol × RNG → (List (Symbol . String))
;;; Generate multiple choice options based on answer type and category
(define (generate-problem-options correct-answer category rng)
  (doc 'export #t)
  (cond
    ;; Numeric answers: use numeric distractors
    [(number? correct-answer)
     (let* ([strategies (category-distractor-strategies category)]
            [result (make-numeric-options correct-answer strategies rng format-numeric-option)])
       (car result))]

    ;; Comparison answers (A/B/same/etc)
    [(symbol? correct-answer)
     (let* ([all-choices (category-comparison-choices category)]
            [result (make-symbolic-options correct-answer all-choices rng)])
       (car result))]

    ;; Default: simple A/B/C/D with shuffled positions
    [else
     (let* ([texts (list (format-answer correct-answer)
                         "Not enough information"
                         "Cannot determine"
                         "None of the above")]
            [shuffled (shuffle-list texts rng)]
            [labels '(A B C D)])
       (map cons labels shuffled))]))

;;; find-correct-label : (List (Symbol . String)) × Any → Symbol
;;; Find which option label corresponds to correct answer
(define (find-correct-label options correct-answer)
  (let ([formatted (format-answer correct-answer)])
    (let loop ([opts options])
      (if (null? opts)
          'A  ; fallback
          (if (string-contains-approx? (cdar opts) formatted)
              (caar opts)
              (loop (cdr opts)))))))

;;; string-contains-approx? : String × String → Boolean
(define (string-contains-approx? haystack needle)
  (or (string=? haystack needle)
      (and (> (string-length haystack) 0)
           (> (string-length needle) 0)
           (let ([hay-num (string->number haystack)]
                 [need-num (string->number needle)])
             (and hay-num need-num
                  (< (abs (- hay-num need-num))
                     (* 0.01 (max (abs hay-num) 1))))))))

;;; format-answer : Any → String
(define (format-answer val)
  (cond
    [(number? val) (format-numeric-option val)]
    [(symbol? val) (symbol->string val)]
    [(string? val) val]
    [else (format "~a" val)]))

;;; category-distractor-strategies : Symbol → (List DistractorStrategy)
;;; Get appropriate distractor strategies for problem category
(define (category-distractor-strategies category)
  (case category
    [(spatial count size distance) counting-distractors]
    [(velocity speed kinematic) velocity-distractors]
    [(mass momentum dynamic) numeric-distractors]
    [else numeric-distractors]))

;;; category-comparison-choices : Symbol → (List Symbol)
;;; Get comparison choices for category
(define (category-comparison-choices category)
  (case category
    [(spatial size) '(A B same cannot-determine)]
    [(velocity speed) '(A B same stationary)]
    [(mass) '(A B equal cannot-determine)]
    [else '(A B C D)]))

;;; ============================================================
;;; Part 4: Batch Generation
;;; ============================================================

(doc 'section 'batch)

;;; generate-problem-batch : PhysicsProblem × RNG × ProblemConfig × Nat → (List Sample)
;;; Generate multiple problem instances
(define (generate-problem-batch problem rng config count)
  (doc 'export #t)
  (let loop ([remaining count] [samples '()])
    (if (<= remaining 0)
        (reverse samples)
        (let ([sample (generate-problem-instance problem rng config)])
          (loop (- remaining 1) (cons sample samples))))))

;;; ============================================================
;;; Part 5: Problem Validation
;;; ============================================================

(doc 'section 'validation)

;;; validate-problem-params : PhysicsProblem × Alist → Boolean
;;; Check if parameters produce a valid, solvable problem
(define (validate-problem-params problem params)
  (doc 'export #t)
  ;; TODO: Add specific validation per category
  ;; For now, just check params exist
  (and (list? params)
       (> (length params) 0)))

;;; check-visual-solvability : (List String) × Any → Boolean
;;; Verify the problem is solvable from the visual frames alone
;;; (This is a placeholder - real implementation would analyze frame content)
(define (check-visual-solvability frames answer)
  (doc 'export #t)
  ;; For now, just check frames exist
  (and (list? frames)
       (> (length frames) 0)
       (andmap string? frames)))

;;; ============================================================
;;; Part 6: Presentation Helpers
;;; ============================================================

(doc 'section 'presentation)

;;; format-frames-for-display : (List String) → String
;;; Format multiple frames for display in a sample
(define (format-frames-for-display frames)
  (doc 'export #t)
  (let* ([n (length frames)]
         [frame-strs (map (lambda (f i)
                            (string-append "Frame " (number->string i) ":\n" f))
                          frames
                          (iota n))])
    (apply string-append
           (map (lambda (s) (string-append s "\n\n"))
                frame-strs))))

;; iota is provided by prelude

;;; andmap : (a → Bool) × (List a) → Bool
(define (andmap pred lst)
  (cond
    [(null? lst) #t]
    [(pred (car lst)) (andmap pred (cdr lst))]
    [else #f]))
