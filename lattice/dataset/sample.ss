;;; lattice/dataset/sample.ss — Sample Record Type with Q/A Lenses
;;; @module sample
;;; @requires prelude optics
;;; @description Sample record type with Q/A lenses into state
;;; @purity partial
;;; @stability experimental

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'optics)
(require 'parameter)

(doc 'module 'sample)
(doc 'description "Sample record type for visual reasoning datasets. Questions and answers are defined as lenses into simulation state, ensuring ground truth consistency by construction.")
(doc 'layer 'lattice)
(doc 'purity 'partial)

;;; ============================================================
;;; Part 1: Sample Record Type
;;; ============================================================
;;;
;;; A sample represents a single problem instance:
;;;   - id: unique identifier for tracking
;;;   - frames: list of ASCII frame strings (the visual input)
;;;   - question: the question text
;;;   - options: multiple choice options (A/B/C/D)
;;;   - answer: correct answer symbol ('A, 'B, 'C, or 'D)
;;;   - metadata: additional info (category, difficulty, params, etc.)

(doc 'section 'record)

;;; make-sample : String × (List String) × String × (List (Symbol . String)) × Symbol × Alist → Sample
(define (make-sample id frames question options answer metadata)
  (doc 'export #t)
  (doc 'type '(-> String (List String) String (List (Pair Symbol String)) Symbol Alist Sample))
  (list 'sample id frames question options answer metadata))

;;; sample? : Any → Boolean
(define (sample? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'sample)))

;;; Accessors
(define (sample-id s) (list-ref s 1))
(doc sample-id 'export #t)
(doc sample-id 'type '(-> Sample String))

(define (sample-frames s) (list-ref s 2))
(doc sample-frames 'export #t)
(doc sample-frames 'type '(-> Sample (List String)))

(define (sample-question s) (list-ref s 3))
(doc sample-question 'export #t)
(doc sample-question 'type '(-> Sample String))

(define (sample-options s) (list-ref s 4))
(doc sample-options 'export #t)
(doc sample-options 'type '(-> Sample (List (Pair Symbol String))))

(define (sample-answer s) (list-ref s 5))
(doc sample-answer 'export #t)
(doc sample-answer 'type '(-> Sample Symbol))

(define (sample-metadata s) (list-ref s 6))
(doc sample-metadata 'export #t)
(doc sample-metadata 'type '(-> Sample Alist))

;;; ============================================================
;;; Part 2: Sample Lenses
;;; ============================================================
;;;
;;; Lenses provide composable access to sample fields.

(doc 'section 'lenses)

;;; sample-id-lens : Lens Sample String
(doc sample-id-lens 'export #t)
(define sample-id-lens
  (make-lens
   sample-id
   (lambda (new-id s)
     (make-sample new-id
                  (sample-frames s)
                  (sample-question s)
                  (sample-options s)
                  (sample-answer s)
                  (sample-metadata s)))))

;;; sample-frames-lens : Lens Sample (List String)
(doc sample-frames-lens 'export #t)
(define sample-frames-lens
  (make-lens
   sample-frames
   (lambda (new-frames s)
     (make-sample (sample-id s)
                  new-frames
                  (sample-question s)
                  (sample-options s)
                  (sample-answer s)
                  (sample-metadata s)))))

;;; sample-question-lens : Lens Sample String
(doc sample-question-lens 'export #t)
(define sample-question-lens
  (make-lens
   sample-question
   (lambda (new-q s)
     (make-sample (sample-id s)
                  (sample-frames s)
                  new-q
                  (sample-options s)
                  (sample-answer s)
                  (sample-metadata s)))))

;;; sample-options-lens : Lens Sample (List (Symbol . String))
(doc sample-options-lens 'export #t)
(define sample-options-lens
  (make-lens
   sample-options
   (lambda (new-opts s)
     (make-sample (sample-id s)
                  (sample-frames s)
                  (sample-question s)
                  new-opts
                  (sample-answer s)
                  (sample-metadata s)))))

;;; sample-answer-lens : Lens Sample Symbol
(doc sample-answer-lens 'export #t)
(define sample-answer-lens
  (make-lens
   sample-answer
   (lambda (new-ans s)
     (make-sample (sample-id s)
                  (sample-frames s)
                  (sample-question s)
                  (sample-options s)
                  new-ans
                  (sample-metadata s)))))

;;; sample-metadata-lens : Lens Sample Alist
(doc sample-metadata-lens 'export #t)
(define sample-metadata-lens
  (make-lens
   sample-metadata
   (lambda (new-meta s)
     (make-sample (sample-id s)
                  (sample-frames s)
                  (sample-question s)
                  (sample-options s)
                  (sample-answer s)
                  new-meta))))

;;; ============================================================
;;; Part 3: Metadata Accessors
;;; ============================================================
;;;
;;; Convenient accessors for common metadata fields.

(doc 'section 'metadata)

;;; sample-category : Sample → Symbol | #f
(define (sample-category s)
  (doc 'export #t)
  (let ([pair (assq 'category (sample-metadata s))])
    (and pair (cdr pair))))

;;; sample-difficulty : Sample → Number | #f
(define (sample-difficulty s)
  (doc 'export #t)
  (let ([pair (assq 'difficulty (sample-metadata s))])
    (and pair (cdr pair))))

;;; sample-params : Sample → Alist | #f
(define (sample-params s)
  (doc 'export #t)
  (let ([pair (assq 'params (sample-metadata s))])
    (and pair (cdr pair))))

;;; sample-with-meta : Sample × Symbol × Any → Sample
(define (sample-with-meta s key value)
  (doc 'export #t)
  (doc 'description "Add or update a metadata field")
  (let* ([meta (sample-metadata s)]
         [filtered (filter (lambda (p) (not (eq? (car p) key))) meta)]
         [new-meta (cons (cons key value) filtered)])
    (set-lens sample-metadata-lens new-meta s)))

;;; ============================================================
;;; Part 4: Q/A Lens Pattern
;;; ============================================================
;;;
;;; Questions and answers are views into simulation state at different times.
;;; This ensures ground truth consistency by construction.
;;;
;;; The pattern:
;;;   - Question lens: view state at t=0 (initial conditions)
;;;   - Answer lens: view state at t=final (simulation outcome)
;;;
;;; Example: For "What is Ball A's final velocity?"
;;;   - Question: frames from t=0..T, question text
;;;   - Answer: computed from state@T via lens

(doc 'section 'qa-lens)

;;; make-qa-lens : (State → Question) × (State → Answer) → QALens
;;; Create a Q/A lens pair that extracts question and answer from simulation state.
(define (make-qa-lens question-extractor answer-extractor)
  (doc 'export #t)
  (doc 'type '(-> (-> State String) (-> State Any) QALens))
  (doc 'description "Create Q/A lens pair. Question extracted from initial state, answer from final state.")
  (list 'qa-lens question-extractor answer-extractor))

;;; qa-lens? : Any → Boolean
(define (qa-lens? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'qa-lens)))

;;; qa-lens-question : QALens → (State → String)
(define (qa-lens-question ql)
  (doc 'export #t)
  (cadr ql))

;;; qa-lens-answer : QALens → (State → Any)
(define (qa-lens-answer ql)
  (doc 'export #t)
  (caddr ql))

;;; qa-extract-question : QALens × State → String
(define (qa-extract-question ql state)
  (doc 'export #t)
  ((qa-lens-question ql) state))

;;; qa-extract-answer : QALens × State → Any
(define (qa-extract-answer ql state)
  (doc 'export #t)
  ((qa-lens-answer ql) state))

;;; ============================================================
;;; Part 5: Multiple Choice Helpers
;;; ============================================================

(doc 'section 'multiple-choice)

;;; make-option : Symbol × String → Option
(define (make-option label text)
  (doc 'export #t)
  (cons label text))

;;; option-label : Option → Symbol
(define (option-label opt) (car opt))
(doc option-label 'export #t)

;;; option-text : Option → String
(define (option-text opt) (cdr opt))
(doc option-text 'export #t)

;;; make-options : String × String × String × String → (List Option)
;;; Create standard A/B/C/D options
(define (make-options a b c d)
  (doc 'export #t)
  (list (make-option 'A a)
        (make-option 'B b)
        (make-option 'C c)
        (make-option 'D d)))

;;; option-by-label : (List Option) × Symbol → String | #f
(define (option-by-label opts label)
  (doc 'export #t)
  (let ([pair (assq label opts)])
    (and pair (cdr pair))))

;;; fisher-yates-shuffle : (List a) × RNG → (List a)
;;; Proper Fisher-Yates shuffle using mutation on a vector
;;; RNG must be a mutable RNG (from make-prng)
(define (fisher-yates-shuffle lst rng)
  (let* ([v (list->vector lst)]
         [n (vector-length v)])
    (let loop ([i (- n 1)])
      (when (> i 0)
        (let* ([j (prng-next-int rng (+ i 1))]
               [tmp (vector-ref v i)])
          (vector-set! v i (vector-ref v j))
          (vector-set! v j tmp)
          (loop (- i 1)))))
    (vector->list v)))

;;; shuffle-options : (List Option) × Symbol × RNG → (List Option) × Symbol
;;; Shuffle options while tracking which one is correct.
;;; Returns (shuffled-options . new-correct-label)
(define (shuffle-options opts correct-label rng)
  (doc 'export #t)
  (let* ([correct-text (option-by-label opts correct-label)]
         [texts (map cdr opts)]
         [shuffled (fisher-yates-shuffle texts rng)]
         [labels '(A B C D)]
         [new-opts (map cons labels shuffled)]
         [new-label (car (filter (lambda (o) (string=? (cdr o) correct-text)) new-opts))])
    (cons new-opts (car new-label))))

;;; ============================================================
;;; Part 6: Sample Rendering
;;; ============================================================

(doc 'section 'rendering)

;;; sample->prompt : Sample → String
;;; Convert sample to prompt string (frames + question + options)
(define (sample->prompt s)
  (doc 'export #t)
  (let* ([frames (sample-frames s)]
         [question (sample-question s)]
         [options (sample-options s)]
         [frame-str (apply string-append
                           (map (lambda (f) (string-append f "\n\n")) frames))]
         [opt-str (apply string-append
                         (map (lambda (o)
                                (string-append (symbol->string (car o))
                                               ") " (cdr o) "\n"))
                              options))])
    (string-append frame-str question "\n\n" opt-str)))

;;; sample->sexp : Sample → Sexp
;;; Convert sample to canonical S-expression format
(define (sample->sexp s)
  (doc 'export #t)
  `(sample
    (id ,(sample-id s))
    (frames ,@(sample-frames s))
    (question ,(sample-question s))
    (options ,@(map (lambda (o) (list (car o) (cdr o))) (sample-options s)))
    (answer ,(sample-answer s))
    (metadata ,@(sample-metadata s))))

;;; sexp->sample : Sexp → Sample
;;; Parse sample from S-expression
(define (sexp->sample sexp)
  (doc 'export #t)
  (if (and (pair? sexp) (eq? (car sexp) 'sample))
      (let* ([fields (cdr sexp)]
             [get (lambda (key) (cdr (assq key fields)))]
             [id (car (get 'id))]
             [frames (get 'frames)]
             [question (car (get 'question))]
             [opts-raw (get 'options)]
             [options (map (lambda (o) (cons (car o) (cadr o))) opts-raw)]
             [answer (car (get 'answer))]
             [metadata (get 'metadata)])
        (make-sample id frames question options answer metadata))
      (error 'sexp->sample "Invalid sample S-expression")))

;;; ============================================================
;;; Part 7: Sample Validation
;;; ============================================================

(doc 'section 'validation)

;;; sample-valid? : Sample → Boolean
;;; Check if sample is structurally valid
(define (sample-valid? s)
  (doc 'export #t)
  (and (sample? s)
       (string? (sample-id s))
       (list? (sample-frames s))
       (andmap string? (sample-frames s))
       (string? (sample-question s))
       (list? (sample-options s))
       (= (length (sample-options s)) 4)
       (memq (sample-answer s) '(A B C D))
       (list? (sample-metadata s))))

;;; sample-answer-in-options? : Sample → Boolean
;;; Verify answer label exists in options
(define (sample-answer-in-options? s)
  (doc 'export #t)
  (let ([opts (sample-options s)]
        [ans (sample-answer s)])
    (and (assq ans opts) #t)))
