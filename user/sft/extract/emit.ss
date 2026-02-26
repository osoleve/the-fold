;;; user/sft/extract/emit.ss — JSONL emission for SFT samples
;;;
;;; Converts internal sample records into the standard JSONL schema used
;;; by generate.ss and all dataset files in data/*.
;;;
;;; Output schema matches existing datasets:
;;;   {id, family, category, difficulty, source_module, source_test,
;;;    source_function, prompt_body, prompt, ground_truth, verify_expr,
;;;    tags, split}
;;;
;;; Usage:
;;;   (load "user/sft/extract/emit.ss")
;;;   (emit-jsonl samples "output.jsonl")
;;;   (emit-jsonl-split samples "output-dir/")  ; writes all.jsonl, train.jsonl, eval.jsonl

(unless (top-level-bound? 'filter-map) (load "core/base/prelude.ss"))
(load "core/base/sha256.ss")
(load "boundary/io/json.ss")

;;; Load the grammar system for prompt diversification
(unless (top-level-bound? '*sft-base-grammar*)
  (load "user/sft/base-grammar.ss"))

;;; ====
;;; S-expression formatting
;;; ====

(define (emit/sexp->string expr)
  ;; Convert an S-expression to a compact string for JSONL fields.
  ;; Uses write semantics for faithful round-tripping.
  (if (not expr) ""
      (let ([port (open-output-string)])
        (write expr port)
        (get-output-string port))))

(define (emit/sexp->pretty-string expr)
  ;; Convert an S-expression to a pretty-printed string.
  ;; Used for ground_truth and verify_expr where readability matters.
  (if (not expr) ""
      (let ([port (open-output-string)])
        (parameterize ([pretty-standard-indent 2]
                       [pretty-maximum-lines #f])
          (pretty-print expr port))
        ;; Strip trailing newline from pretty-print
        (let* ([s (get-output-string port)]
               [n (string-length s)])
          (if (and (> n 0) (char=? (string-ref s (- n 1)) #\newline))
              (substring s 0 (- n 1))
              s)))))

;;; ====
;;; ID generation
;;; ====

(define *emit-counter* 0)

(define (emit/next-id prefix)
  ;; Generate a unique ID: prefix_NNN
  (set! *emit-counter* (+ *emit-counter* 1))
  (format "~a_~3,'0d" prefix *emit-counter*))

(define (emit/reset-counter!)
  (set! *emit-counter* 0))

;;; ====
;;; Split assignment — by FUNCTION, not by sample
;;; ====
;;;
;;; All variants of the same function (type+desc, test-spec, skeleton,
;;; cloze, type-inhabit, etc.) must land in the same split. Otherwise
;;; the model sees the same function in both train and eval (92.4% leakage).
;;; We hash (source_module, source_function) to assign splits deterministically.

(define *emit-eval-ratio* 0.05)

(define (emit/id->seed id-string)
  ;; Deterministic 32-bit seed from sample ID via SHA256.
  (let* ([hex (sha256-hex (string->utf8 id-string))]
         [first8 (substring hex 0 8)])
    (string->number first8 16)))

(define (emit/function-key->seed source-module source-function)
  ;; Deterministic seed from (source_module, source_function) pair.
  ;; Uses both fields to avoid collisions from same-named functions
  ;; in different modules.
  (let* ([key (string-append source-module ":" source-function)]
         [hex (sha256-hex (string->utf8 key))]
         [first8 (substring hex 0 8)])
    (string->number first8 16)))

(define (emit/seed->split seed)
  ;; Deterministic train/eval split.
  (if (< (/ (modulo seed 10000) 10000.0) *emit-eval-ratio*)
      "eval"
      "train"))

;;; ====
;;; Complexity-based difficulty assignment
;;; ====

(define (emit/sexp-size sexp)
  ;; Count atoms in an S-expression tree.
  (cond
    [(pair? sexp) (+ (emit/sexp-size (car sexp))
                     (emit/sexp-size (cdr sexp)))]
    [(null? sexp) 0]
    [else 1]))

(define (emit/count-branches sexp)
  ;; Count branching forms (if, cond clauses, case clauses, match clauses).
  (cond
    [(not (pair? sexp)) 0]
    [(eq? (car sexp) 'if)
     (+ 1 (emit/count-branches-list (cdr sexp)))]
    [(memq (car sexp) '(cond case match))
     (+ (max 0 (- (length (cdr sexp)) 1))  ; each clause past the first
        (emit/count-branches-list (cdr sexp)))]
    [else (emit/count-branches-list sexp)]))

(define (emit/count-branches-list lst)
  (if (pair? lst)
      (+ (emit/count-branches (car lst))
         (emit/count-branches-list (cdr lst)))
      0))

(define (emit/compute-difficulty body-sexp cross-dep-count)
  ;; Assign difficulty based on function complexity metrics.
  ;;   easy:   body size <= 10, <= 1 branch
  ;;   medium: body size 11-30 or 2-3 branches
  ;;   hard:   body size > 30 or 4+ branches or 3+ cross-module deps
  (let* ([size (emit/sexp-size body-sexp)]
         [branches (emit/count-branches body-sexp)])
    (cond
      [(or (> size 30) (>= branches 4) (>= cross-dep-count 3)) "hard"]
      [(or (> size 10) (>= branches 2)) "medium"]
      [else "easy"])))

(define (emit/ir-difficulty ir)
  ;; Compute difficulty from a walker IR record.
  (let* ([body (let ([b (assq 'body ir)]) (if b (cdr b) '()))]
         [body-sexp (if (and (pair? body) (= (length body) 1))
                        (car body)
                        (if (pair? body) (cons 'begin body) '()))]
         [deps (let ([d (assq 'deps ir)]) (if d (cdr d) '()))])
    (emit/compute-difficulty body-sexp (length deps))))

;;; ====
;;; Mutation-specific difficulty grading
;;; ====

(define (emit/mutation-difficulty mutation-desc)
  ;; Grade bugfix difficulty by mutation type.
  ;;   easy:   accessor swap, arithmetic swap
  ;;   medium: equality swap, branch swap, accumulator init
  ;;   hard:   base case delete, off-by-one, missing recursion
  (cond
    [(or (string-contains mutation-desc "Accessor")
         (string-contains mutation-desc "Arithmetic"))
     "easy"]
    [(or (string-contains mutation-desc "Base case")
         (string-contains mutation-desc "Off-by-one")
         (string-contains mutation-desc "Missing recursion"))
     "hard"]
    [else "medium"]))

(define (string-contains haystack needle)
  ;; Simple substring search.
  (let ([hl (string-length haystack)]
        [nl (string-length needle)])
    (let loop ([i 0])
      (cond
        [(> (+ i nl) hl) #f]
        [(string=? (substring haystack i (+ i nl)) needle) #t]
        [else (loop (+ i 1))]))))

;;; ====
;;; Tag generation
;;; ====

(define (emit/make-tags skill module family fn-name . extra)
  ;; Build standard tag list for a sample.
  ;; Accepts both symbols and strings for skill/module (some modules use string tags).
  (let* ([to-string (lambda (x)
                      (cond [(symbol? x) (symbol->string x)]
                            [(string? x) x]
                            [else (format "~a" x)]))]
         [base (filter (lambda (x) (or (symbol? x) (string? x)))
                  (list skill module
                        (string->symbol family)
                        fn-name))])
    (append (map to-string base)
            (if (pair? extra) (car extra) '()))))

;;; ====
;;; Prompt expansion
;;; ====

(define (emit/expand-prompt family category fn-name prompt-body
                            verify-expr-str available-fns known-issue seed)
  ;; Expand prompt-body through the grammar DSL for diversification.
  (let ([ctx (make-sft-context family category
               (symbol->string fn-name) prompt-body
               verify-expr-str available-fns known-issue)])
    (expand-to-string *sft-base-grammar* 'prompt ctx seed)))

;;; ====
;;; Sample construction
;;; ====

(define (make-sample id family category difficulty
                     source-module source-test source-function
                     prompt-body ground-truth-str verify-expr-str
                     tags . opts)
  ;; Build a complete sample alist in standard schema.
  ;; Split is assigned by FUNCTION (source_module + source_function),
  ;; not by sample ID, to prevent cross-split leakage.
  (let* ([fn-str (if (symbol? source-function)
                     (symbol->string source-function)
                     source-function)]
         [seed (emit/id->seed id)]
         [split (emit/seed->split
                  (emit/function-key->seed source-module fn-str))]
         [available-fns (if (pair? opts) (car opts) #f)]
         [known-issue (if (and (pair? opts) (pair? (cdr opts)))
                          (cadr opts) #f)]
         [prompt (guard (exn [else prompt-body])
                   (emit/expand-prompt
                     family category
                     (if (symbol? source-function)
                         source-function
                         (string->symbol source-function))
                     prompt-body verify-expr-str
                     available-fns known-issue seed))])
    `((id . ,id)
      (family . ,family)
      (category . ,category)
      (difficulty . ,difficulty)
      (source_module . ,source-module)
      (source_test . ,source-test)
      (source_function . ,(if (symbol? source-function)
                              (symbol->string source-function)
                              source-function))
      (prompt_body . ,prompt-body)
      (prompt . ,prompt)
      (ground_truth . ,ground-truth-str)
      (verify_expr . ,(or verify-expr-str ""))
      (tags . ,tags)
      (split . ,split))))

;;; ====
;;; File output
;;; ====

;; Internal fields that should not appear in JSONL output
(define *emit-internal-keys*
  '(ground_truth_sexp verify_sexp skill module))

(define (emit/strip-internal sample)
  ;; Remove internal sexp-valued fields before JSON serialization.
  (remp (lambda (pair) (memq (car pair) *emit-internal-keys*)) sample))

(define (emit/deduplicate samples)
  ;; Remove samples with exact duplicate prompt_body fields.
  ;; Uses SHA256 hash of prompt_body for efficient dedup.
  (let loop ([remaining samples] [seen '()] [acc '()] [dupes 0])
    (if (null? remaining)
        (begin
          (when (> dupes 0)
            (printf "Deduplicated: removed ~a exact prompt duplicates\n" dupes))
          (reverse acc))
        (let* ([s (car remaining)]
               [body (cdr (assq 'prompt_body s))]
               [hash (sha256-hex (string->utf8 body))]
               [first8 (substring hash 0 16)])
          (if (member first8 seen)
              (loop (cdr remaining) seen acc (+ dupes 1))
              (loop (cdr remaining) (cons first8 seen)
                    (cons s acc) dupes))))))

(define (emit-jsonl samples path)
  ;; Write samples as JSONL to a single file.
  ;; Strips internal fields (sexp-valued keys) before serialization.
  (call-with-port
    (open-file-output-port path
      (file-options no-fail) (buffer-mode block)
      (make-transcoder (utf-8-codec)))
    (lambda (port)
      (for-each (lambda (sample)
                  (put-string port (json->string (emit/strip-internal sample)))
                  (put-string port "\n"))
                samples)))
  (printf "Wrote ~a samples to ~a\n" (length samples) path))

(define (emit-jsonl-split samples output-dir)
  ;; Write samples split into all.jsonl, train.jsonl, eval.jsonl.
  (let ([trains (filter (lambda (r) (string=? (cdr (assq 'split r)) "train"))
                        samples)]
        [evals (filter (lambda (r) (string=? (cdr (assq 'split r)) "eval"))
                       samples)])
    (emit-jsonl samples (string-append output-dir "/all.jsonl"))
    (emit-jsonl trains  (string-append output-dir "/train.jsonl"))
    (emit-jsonl evals   (string-append output-dir "/eval.jsonl"))
    (printf "Split: ~a train, ~a eval\n" (length trains) (length evals))))

;;; ====
;;; Convenience: IR → sample conversion
;;; ====

(define (ir->sample-base ir family category difficulty id-prefix)
  ;; Convert a walker IR record into sample skeleton with standard metadata.
  ;; Does NOT fill prompt_body, ground_truth, verify_expr — those are family-specific.
  ;; Returns a partial alist for the downstream extractor to complete.
  (let* ([name (cdr (assq 'name ir))]
         [skill (cdr (assq 'skill ir))]
         [module (cdr (assq 'module ir))]
         [file (cdr (assq 'file ir))]
         [tests (let ([t (assq 'tests ir)]) (if t (cdr t) '()))]
         [test-file (if (pair? tests) file "")]
         [id (emit/next-id (format "~a_~a_~a"
                                    (symbol->string skill)
                                    family
                                    (symbol->string name)))])
    `((id . ,id)
      (family . ,family)
      (category . ,category)
      (difficulty . ,difficulty)
      (source_module . ,file)
      (source_test . ,test-file)
      (source_function . ,(symbol->string name))
      (skill . ,skill)
      (module . ,module)
      (tags . ,(emit/make-tags skill module family name)))))

;;; ====
;;; REPL interface
;;; ====

(printf "emit.ss loaded.\n")
(printf "  (make-sample id fam cat diff mod test fn body gt ve tags) - Build sample\n")
(printf "  (emit-jsonl samples path)        - Write JSONL\n")
(printf "  (emit-jsonl-split samples dir)   - Write train/eval split\n")
(printf "  (ir->sample-base ir fam cat diff prefix) - IR to sample skeleton\n")
(printf "  (emit/sexp->string expr)         - S-exp to compact string\n")
(printf "  (emit/sexp->pretty-string expr)  - S-exp to pretty string\n")
