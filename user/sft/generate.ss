;;; user/sft/generate.ss — Batch JSONL prompt generation
;;;
;;; Reads pre-diversification JSONL (with prompt_body), expands each
;;; record through the grammar DSL with a deterministic seed derived
;;; from the sample ID, and emits output JSONL (all/train/eval splits).
;;;
;;; Usage:
;;;   scheme --script user/sft/generate.ss input.jsonl output-dir/
;;;
;;; Environment:
;;;   SFT_EVAL_RATIO  — eval split fraction (default: 0.05)
;;;   SFT_GRAMMAR     — path to grammar overlay file (optional)

(load "core/base/sha256.ss")
(load "boundary/io/json.ss")
(load "user/sft/base-grammar.ss")

;;; --- Configuration ---

(define *eval-ratio*
  (let ([env (getenv "SFT_EVAL_RATIO")])
    (if env
        (or (string->number env) 0.05)
        0.05)))

(define *grammar-overlay-path* (getenv "SFT_GRAMMAR"))

(define *active-grammar*
  (if *grammar-overlay-path*
      (let ([overlay (load-grammar-overlay *grammar-overlay-path*)])
        (grammar-merge *sft-base-grammar* overlay))
      *sft-base-grammar*))

(define (load-grammar-overlay path)
  (doc 'description "Load a grammar overlay from a file defining *grammar-overlay*")
  (load path)
  (if (top-level-bound? '*grammar-overlay*)
      (eval '*grammar-overlay*)
      (error 'load-grammar-overlay
             "overlay file must define *grammar-overlay*" path)))

;;; --- Seed derivation ---

(define (id->seed id-string)
  (doc 'type '(-> String Int))
  (doc 'description "Deterministic 32-bit seed from sample ID via SHA256")
  (let* ([hex (sha256-hex (string->utf8 id-string))]
         [first8 (substring hex 0 8)])
    (string->number first8 16)))

;;; --- Split ---

(define (seed->split seed)
  (doc 'type '(-> Int String))
  (doc 'description "Deterministic train/eval split from seed")
  (if (< (/ (modulo seed 10000) 10000.0) *eval-ratio*)
      "eval"
      "train"))

;;; --- Record helpers ---

(define (assoc-default key alist default)
  (let ([entry (assq key alist)])
    (if entry (cdr entry) default)))

(define (require-field key record id)
  (let ([entry (assq key record)])
    (unless entry
      (error 'process-record
             (format "missing required field '~a'" key)
             id))
    (cdr entry)))

;;; --- Core processing ---

(define (valid-split? x)
  (and (string? x)
       (or (string=? x "train")
           (string=? x "eval"))))

(define (process-record record)
  (doc 'description "Transform a pre-diversification record into output format")
  (let* ([id          (require-field 'id record "<unknown>")]
         [family      (require-field 'family record id)]
         [category    (require-field 'category record id)]
         [fn-name     (require-field 'source_function record id)]
         [prompt-body
          (let ([pb (assoc-default 'prompt_body record #f)]
                [p  (assoc-default 'prompt record #f)])
            (cond
              [(and pb (string? pb) (not (string=? pb ""))) pb]
              [(and p (string? p) (not (string=? p ""))) p]
              [else
               (error 'process-record
                      "missing required field 'prompt_body' (or fallback 'prompt')"
                      id)]))]
         [ground-truth (require-field 'ground_truth record id)]
         [verify-raw  (assoc-default 'verify_expr record #f)]
         [avail-fns   (assoc-default 'available_functions record #f)]
         [known-issue (assoc-default 'known_issue record #f)]
         [seed        (id->seed id)]
         [ctx         (make-sft-context family category fn-name prompt-body
                        verify-raw avail-fns known-issue)]
         [prompt      (expand-to-string *active-grammar* 'prompt ctx seed)]
         [input-split (assoc-default 'split record #f)]
         [split       (if (valid-split? input-split)
                          input-split
                          (seed->split seed))])
    `((id . ,id)
      (family . ,family)
      (category . ,category)
      (difficulty . ,(assoc-default 'difficulty record "medium"))
      (source_module . ,(assoc-default 'source_module record ""))
      (source_test . ,(assoc-default 'source_test record ""))
      (source_function . ,fn-name)
      (prompt_body . ,prompt-body)
      (prompt . ,prompt)
      (ground_truth . ,ground-truth)
      (verify_expr . ,(or verify-raw ""))
      (tags . ,(assoc-default 'tags record '()))
      (split . ,split))))

;;; --- I/O ---

(define (warn msg . args)
  (apply fprintf (current-error-port)
         (string-append "warning: " msg "~%") args))

(define (read-jsonl path)
  (doc 'description "Read JSONL file, return list of parsed alists")
  (call-with-port
    (open-file-input-port path
      (file-options) (buffer-mode block)
      (make-transcoder (utf-8-codec)))
    (lambda (port)
      (let loop ([acc '()])
        (let ([line (get-line port)])
          (if (eof-object? line)
              (reverse acc)
              (if (string=? (json-string-trim line) "")
                  (loop acc)  ; skip blank lines
                  (let ([parsed (parse-json-string line)])
                    (if parsed
                        (loop (cons parsed acc))
                        (begin
                          (warn "malformed JSON, skipping: ~a"
                                (if (> (string-length line) 80)
                                    (string-append (substring line 0 80) "...")
                                    line))
                          (loop acc)))))))))))

(define (write-jsonl records path)
  (doc 'description "Write records as JSONL to path")
  (call-with-port
    (open-file-output-port path
      (file-options no-fail) (buffer-mode block)
      (make-transcoder (utf-8-codec)))
    (lambda (port)
      (for-each (lambda (record)
                  (put-string port (json->string record))
                  (put-string port "\n"))
                records))))

;;; --- Main ---

(define (generate input-path output-dir)
  (doc 'description "Main generation pipeline: read, expand, write splits")
  (let* ([records (read-jsonl input-path)]
         [results
          (let loop ([rs records] [acc '()] [ok 0] [skip 0])
            (if (null? rs)
                (begin
                  (fprintf (current-error-port)
                           "processed ~a records (~a ok, ~a skipped)~%"
                           (+ ok skip) ok skip)
                  (reverse acc))
                (guard (ex [else
                        (warn "error processing record: ~a"
                              (if (condition? ex)
                                  (condition-message ex)
                                  ex))
                        (loop (cdr rs) acc ok (+ skip 1))])
                  (let ([out (process-record (car rs))])
                    (loop (cdr rs) (cons out acc) (+ ok 1) skip)))))]
         [trains (filter (lambda (r) (string=? (cdr (assq 'split r)) "train"))
                         results)]
         [evals  (filter (lambda (r) (string=? (cdr (assq 'split r)) "eval"))
                         results)])
    ;; Write output files
    (write-jsonl results (string-append output-dir "/all.jsonl"))
    (write-jsonl trains  (string-append output-dir "/train.jsonl"))
    (write-jsonl evals   (string-append output-dir "/eval.jsonl"))
    (fprintf (current-error-port)
             "wrote ~a total (~a train, ~a eval) to ~a~%"
             (length results) (length trains) (length evals)
             output-dir)))

;;; --- CLI entry ---

(let ([args (command-line-arguments)])
  (when (< (length args) 2)
    (fprintf (current-error-port)
             "usage: scheme --script user/sft/generate.ss input.jsonl output-dir/~%")
    (exit 1))
  (let ([input-path (car args)]
        [output-dir (cadr args)])
    (unless (file-exists? input-path)
      (fprintf (current-error-port) "error: input file not found: ~a~%" input-path)
      (exit 1))
    (unless (file-directory? output-dir)
      (fprintf (current-error-port) "error: output directory not found: ~a~%" output-dir)
      (exit 1))
    (generate input-path output-dir)))
