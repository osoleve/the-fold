;;; lattice/dataset/format/jsonl.ss — JSONL Export via Bidirectional Optics
;;; @module jsonl
;;; @requires prelude optics sample

(load "core/base/prelude.ss")
(load "lattice/fp/optics/optics.ss")
(load "lattice/dataset/sample.ss")

(doc 'module 'jsonl)
(doc 'description "Bidirectional JSONL export for ML pipelines. Uses optics for lossless round-trip conversion between S-expression (canonical) and JSON.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================
;;; Part 1: JSON Representation
;;; ============================================================
;;;
;;; We represent JSON as S-expressions:
;;;   - Objects: (json-object (key1 . val1) (key2 . val2) ...)
;;;   - Arrays: (json-array val1 val2 ...)
;;;   - Strings: "string"
;;;   - Numbers: 123, 3.14
;;;   - Booleans: #t, #f
;;;   - Null: 'null

(doc 'section 'json-rep)

;;; make-json-object : (List (String . Any)) → JsonObject
(define (make-json-object pairs)
  (doc 'export #t)
  (cons 'json-object pairs))

(define (json-object? x)
  (and (pair? x) (eq? (car x) 'json-object)))

(define (json-object-pairs obj) (cdr obj))

;;; make-json-array : (List Any) → JsonArray
(define (make-json-array items)
  (doc 'export #t)
  (cons 'json-array items))

(define (json-array? x)
  (and (pair? x) (eq? (car x) 'json-array)))

(define (json-array-items arr) (cdr arr))

;;; ============================================================
;;; Part 2: Sample ↔ JSON Conversion
;;; ============================================================

(doc 'section 'conversion)

;;; sample->json : Sample → JsonObject
(define (sample->json s)
  (doc 'export #t)
  (make-json-object
   `(("id" . ,(sample-id s))
     ("frames" . ,(make-json-array (sample-frames s)))
     ("question" . ,(sample-question s))
     ("options" . ,(make-json-object
                   (map (lambda (opt)
                         (cons (symbol->string (car opt)) (cdr opt)))
                       (sample-options s))))
     ("answer" . ,(symbol->string (sample-answer s)))
     ("metadata" . ,(make-json-object
                    (map (lambda (m)
                          (cons (symbol->string (car m))
                                (value->json (cdr m))))
                        (sample-metadata s)))))))

;;; json->sample : JsonObject → Sample
(define (json->sample json)
  (doc 'export #t)
  (if (json-object? json)
      (let* ([pairs (json-object-pairs json)]
             [get (lambda (key)
                   (let ([pair (assoc key pairs)])
                     (and pair (cdr pair))))]
             [id (get "id")]
             [frames-json (get "frames")]
             [frames (if (json-array? frames-json)
                        (json-array-items frames-json)
                        '())]
             [question (get "question")]
             [opts-json (get "options")]
             [options (if (json-object? opts-json)
                         (map (lambda (p)
                               (cons (string->symbol (car p)) (cdr p)))
                             (json-object-pairs opts-json))
                         '())]
             [answer (string->symbol (get "answer"))]
             [meta-json (get "metadata")]
             [metadata (if (json-object? meta-json)
                          (map (lambda (p)
                                (cons (string->symbol (car p))
                                      (json->value (cdr p))))
                              (json-object-pairs meta-json))
                          '())])
        (make-sample id frames question options answer metadata))
      (error 'json->sample "Expected JSON object")))

;;; value->json : Any → JsonValue
(define (value->json v)
  (cond
    [(list? v) (make-json-array (map value->json v))]
    [(pair? v) (make-json-object
               (list (cons (symbol->string (car v))
                          (value->json (cdr v)))))]
    [else v]))

;;; json->value : JsonValue → Any
(define (json->value v)
  (cond
    [(json-array? v) (map json->value (json-array-items v))]
    [(json-object? v)
     (map (lambda (p)
           (cons (string->symbol (car p))
                 (json->value (cdr p))))
         (json-object-pairs v))]
    [else v]))

;;; ============================================================
;;; Part 3: Sample Iso (Bidirectional)
;;; ============================================================

(doc 'section 'iso)

;;; sample-json-iso : Iso Sample JsonObject
;;; Bidirectional conversion between Sample and JSON
(doc sample-json-iso 'export #t)
(define sample-json-iso
  (make-iso sample->json json->sample))

;;; iso-roundtrip-check : Sample → Boolean
;;; Verify lossless roundtrip
(define (iso-roundtrip-check s)
  (doc 'export #t)
  (let* ([json (sample->json s)]
         [back (json->sample json)])
    (and (string=? (sample-id s) (sample-id back))
         (equal? (sample-frames s) (sample-frames back))
         (string=? (sample-question s) (sample-question back))
         (eq? (sample-answer s) (sample-answer back)))))

;;; ============================================================
;;; Part 4: JSON Serialization (to String)
;;; ============================================================

(doc 'section 'serialization)

;;; json->string : JsonValue → String
;;; Serialize JSON to string
(define (json->string v)
  (doc 'export #t)
  (cond
    [(json-object? v)
     (let ([pairs (json-object-pairs v)])
       (string-append
        "{"
        (join-strings
         (map (lambda (p)
               (string-append
                (json-string-escape (car p))
                ":"
                (json->string (cdr p))))
             pairs)
         ",")
        "}"))]

    [(json-array? v)
     (string-append
      "["
      (join-strings (map json->string (json-array-items v)) ",")
      "]")]

    [(string? v)
     (json-string-escape v)]

    [(number? v)
     ;; Preserve integer vs float distinction
     (if (and (exact? v) (integer? v))
         (number->string v)
         (number->string (exact->inexact v)))]

    [(boolean? v)
     (if v "true" "false")]

    [(eq? v 'null)
     "null"]

    [(symbol? v)
     (json-string-escape (symbol->string v))]

    [else
     (json-string-escape (format "~a" v))]))

;;; json-string-escape : String → String
;;; Escape string for JSON
(define (json-string-escape s)
  (string-append
   "\""
   (list->string
    (append-map (lambda (c)
                 (cond
                   [(char=? c #\") '(#\\ #\")]
                   [(char=? c #\\) '(#\\ #\\)]
                   [(char=? c #\newline) '(#\\ #\n)]
                   [(char=? c #\return) '(#\\ #\r)]
                   [(char=? c #\tab) '(#\\ #\t)]
                   [else (list c)]))
               (string->list s)))
   "\""))

;;; join-strings : (List String) × String → String
(define (join-strings strs sep)
  (if (null? strs)
      ""
      (let loop ([rest (cdr strs)] [acc (car strs)])
        (if (null? rest)
            acc
            (loop (cdr rest) (string-append acc sep (car rest)))))))

;;; ============================================================
;;; Part 5: JSONL Format
;;; ============================================================

(doc 'section 'jsonl)

;;; samples->jsonl : (List Sample) → String
;;; Convert samples to JSONL (one JSON object per line)
;;; Uses iterative join to handle large datasets (avoids apply limit)
(define (samples->jsonl samples)
  (doc 'export #t)
  (join-strings
   (map (lambda (s)
          (string-append (json->string (sample->json s)) "\n"))
        samples)
   ""))

;;; sample->jsonl-line : Sample → String
;;; Convert single sample to JSONL line
(define (sample->jsonl-line s)
  (doc 'export #t)
  (string-append (json->string (sample->json s)) "\n"))

;;; ============================================================
;;; Part 6: Prompt Format (for LLM APIs)
;;; ============================================================

(doc 'section 'prompt-format)

;;; sample->prompt-json : Sample → JsonObject
;;; Format for LLM prompt (messages format)
(define (sample->prompt-json s)
  (doc 'export #t)
  (let ([prompt-text (sample->prompt s)])
    (make-json-object
     `(("id" . ,(sample-id s))
       ("prompt" . ,prompt-text)
       ("expected" . ,(symbol->string (sample-answer s)))
       ("metadata" . ,(make-json-object
                      (map (lambda (m)
                            (cons (symbol->string (car m))
                                  (value->json (cdr m))))
                          (sample-metadata s))))))))

;;; samples->prompt-jsonl : (List Sample) → String
;;; Convert samples to prompt JSONL format
(define (samples->prompt-jsonl samples)
  (doc 'export #t)
  (join-strings
   (map (lambda (s)
          (string-append (json->string (sample->prompt-json s)) "\n"))
        samples)
   ""))

;;; ============================================================
;;; Part 7: OpenAI-Compatible Format
;;; ============================================================

(doc 'section 'openai-format)

;;; sample->openai-json : Sample → JsonObject
;;; Format compatible with OpenAI fine-tuning API
(define (sample->openai-json s)
  (doc 'export #t)
  (let ([prompt (sample->prompt s)]
        [answer (symbol->string (sample-answer s))])
    (make-json-object
     `(("messages" . ,(make-json-array
                      (list
                       (make-json-object
                        '(("role" . "user")
                          ("content" . ,prompt)))
                       (make-json-object
                        `(("role" . "assistant")
                          ("content" . ,answer))))))))))

;;; samples->openai-jsonl : (List Sample) → String
;;; Convert samples to OpenAI fine-tuning JSONL
(define (samples->openai-jsonl samples)
  (doc 'export #t)
  (join-strings
   (map (lambda (s)
          (string-append (json->string (sample->openai-json s)) "\n"))
        samples)
   ""))
