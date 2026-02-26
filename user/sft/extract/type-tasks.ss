;;; user/sft/extract/type-tasks.ss — Type-driven SFT sample extraction
;;;
;;; Generates tasks centered on type information:
;;;   1. Type inhabitance: Given only type signature, implement function
;;;      (harder than spec-to-code — no natural language description)
;;;   2. Signature prediction: Given function body, predict its type
;;;      (reverse direction — tests type comprehension)
;;;
;;; Requires walker.ss, emit.ss, spec-to-code.ss loaded first.
;;;
;;; Usage:
;;;   (load "user/sft/extract/walker.ss")
;;;   (load "user/sft/extract/emit.ss")
;;;   (load "user/sft/extract/spec-to-code.ss")
;;;   (load "user/sft/extract/type-tasks.ss")
;;;   (define ir (walk-all-manifests))
;;;   (define samples (generate-type-task-samples ir))

;;; ====
;;; Dependencies
;;; ====

(unless (top-level-bound? 'walk-all-manifests)
  (error 'type-tasks "walker.ss must be loaded first"))
(unless (top-level-bound? 'make-sample)
  (error 'type-tasks "emit.ss must be loaded first"))
(unless (top-level-bound? 's2c/build-verify-expr)
  (error 'type-tasks "spec-to-code.ss must be loaded first"))

;;; ====
;;; Type inhabitance: type only → implement
;;; ====

(define (type/inhabitance-sample ir)
  ;; Given ONLY a type signature (no description), implement the function.
  ;; More challenging than spec-to-code: the model must understand the type.
  (let ([type-sig (cdr (assq 'type ir))]
        [name (cdr (assq 'name ir))]
        [skill (cdr (assq 'skill ir))]
        [module (cdr (assq 'module ir))]
        [file (cdr (assq 'file ir))]
        [formals (cdr (assq 'formals ir))]
        [clean-define (cdr (assq 'clean-define ir))]
        [tests (cdr (assq 'tests ir))])
    (and type-sig
         (or (pair? formals) (symbol? formals))
         (let* ([base-diff (emit/ir-difficulty ir)]
                ;; Type inhabitance is always at least medium — no description given
                [difficulty (if (string=? base-diff "easy") "medium" base-diff)]
                [skill-desc (s2c/skill-description skill)]
                [flat-formals (walker/flatten-formals formals)]
                [sig-str (format "(~a~a)"
                                 name
                                 (apply string-append
                                   (map (lambda (f) (format " ~a" f))
                                        flat-formals)))]
                [id (emit/next-id (format "~a_type_inhabit_~a" skill name))]
                [prompt-body
                  (format "Implement a Fold Scheme function that inhabits the following type.\n\nModule: ~a (~a)\nFunction: `~a`\nType: ~a\nParameters: ~a\n\nThe implementation must be consistent with the type contract.\nReturn only the complete function definition, no explanation."
                          file skill-desc sig-str
                          (s2c/format-type-sig type-sig)
                          (apply string-append
                            (map (lambda (f) (format "~a " f)) flat-formals)))]
                [gt-str (emit/sexp->pretty-string clean-define)]
                [ve (s2c/build-verify-expr ir)]
                [ve-str (if ve (emit/sexp->pretty-string ve) "")]
                [tags (emit/make-tags skill module "type-inhabit" name)]
                [sample (make-sample id "type_inhabit" "implementation"
                                     difficulty file
                                     (if (pair? tests) file "")
                                     name prompt-body gt-str ve-str tags)])
           (append sample
                   `((ground_truth_sexp . ,clean-define)
                     (verify_sexp . ,ve)))))))

;;; ====
;;; Signature prediction: body → type
;;; ====

(define (type/signature-sample ir)
  ;; Given the function definition, predict its type signature.
  ;; The model must understand what the function does to write the type.
  (let ([type-sig (cdr (assq 'type ir))]
        [name (cdr (assq 'name ir))]
        [skill (cdr (assq 'skill ir))]
        [module (cdr (assq 'module ir))]
        [file (cdr (assq 'file ir))]
        [formals (cdr (assq 'formals ir))]
        [clean-define (cdr (assq 'clean-define ir))]
        [tests (cdr (assq 'tests ir))])
    (and type-sig
         (or (pair? formals) (symbol? formals))
         (let* ([difficulty (emit/ir-difficulty ir)]
                [skill-desc (s2c/skill-description skill)]
                [body-str (emit/sexp->pretty-string clean-define)]
                [id (emit/next-id (format "~a_type_sig_~a" skill name))]
                [prompt-body
                  (format "What is the type signature for this Fold Scheme function?\n\nModule: ~a (~a)\n\n```scheme\n~a\n```\n\nWrite the type annotation as a doc form.\nReturn only the doc form, e.g.: `(doc ~a 'type (-> Input Output))`"
                          file skill-desc body-str name)]
                ;; Ground truth: the doc form
                [gt-doc `(doc ,name (quote type) ,type-sig)]
                [gt-str (emit/sexp->pretty-string gt-doc)]
                ;; Signature prediction isn't execution-verifiable
                [tags (emit/make-tags skill module "type-sig" name)]
                [sample (make-sample id "type_sig" "analysis"
                                     difficulty file
                                     (if (pair? tests) file "")
                                     name prompt-body gt-str "" tags)])
           sample))))

;;; ====
;;; Main generator
;;; ====

(define (generate-type-task-samples ir-records . opts)
  ;; Generate type-driven task samples from walker IR.
  (let* ([exported-only? (if (pair? opts) (car opts) #t)]
         [typed-candidates (filter
                             (lambda (ir)
                               (and (if exported-only? (cdr (assq 'exported? ir)) #t)
                                    (cdr (assq 'type ir))
                                    (or (pair? (cdr (assq 'formals ir)))
                                        (symbol? (cdr (assq 'formals ir))))))
                             ir-records)]
         [with-tests (filter
                       (lambda (ir) (pair? (cdr (assq 'tests ir))))
                       typed-candidates)]
         [_ (emit/reset-counter!)]
         [_ (printf "Generating type-task samples: ~a typed candidates (~a with tests)...\n"
                    (length typed-candidates) (length with-tests))])
    (let ([inhabit-samples '()]
          [sig-samples '()]
          [inhabit-count 0]
          [sig-count 0])

      ;; 1. Type inhabitance — only functions with tests (execution-verifiable)
      (for-each
        (lambda (ir)
          (guard (exn [else #f])
            (let ([sample (type/inhabitance-sample ir)])
              (when sample
                (set! inhabit-samples (cons sample inhabit-samples))
                (set! inhabit-count (+ inhabit-count 1))))))
        with-tests)

      ;; 2. Signature prediction — all typed, exported functions
      (for-each
        (lambda (ir)
          (guard (exn [else #f])
            (let ([sample (type/signature-sample ir)])
              (when sample
                (set! sig-samples (cons sample sig-samples))
                (set! sig-count (+ sig-count 1))))))
        typed-candidates)

      (let ([all (append (reverse inhabit-samples) (reverse sig-samples))])
        (printf "Generated ~a type-task samples:\n" (length all))
        (printf "  Type inhabitance:     ~a (execution-verifiable)\n" inhabit-count)
        (printf "  Signature prediction: ~a (string-matched)\n" sig-count)
        all))))

;;; ====
;;; REPL interface
;;; ====

(printf "type-tasks.ss loaded.\n")
(printf "  (generate-type-task-samples ir)       - Generate from walker IR\n")
(printf "  (generate-type-task-samples ir #f)    - Generate from ALL functions\n")
