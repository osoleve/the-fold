#!/usr/bin/env scheme-script
;;; export-lattice-meta.ss — Export lattice metadata to JSON for Prime Lab reward functions
;;;
;;; Output: user/rlm/curriculum/lattice-meta.json
;;;
;;; The JSON contains an array of skill objects, each with:
;;;   name, description, keywords, aliases, exports, dependencies
;;;
;;; This powers the BM25 index in the Tier 1 module discovery reward function.

(load "lattice/meta/meta.ss")

;; ---- Minimal JSON emitter ----

(define (json-escape s)
  (if (not (string? s)) (json-escape (format "~a" s))
      (let loop ([i 0] [acc '()])
        (if (>= i (string-length s))
            (list->string (reverse acc))
            (let ([c (string-ref s i)])
              (cond
                [(char=? c #\") (loop (+ i 1) (append '(#\" #\\) acc))]
                [(char=? c #\\) (loop (+ i 1) (append '(#\\ #\\) acc))]
                [(char=? c #\newline) (loop (+ i 1) (append '(#\n #\\) acc))]
                [(char=? c #\tab) (loop (+ i 1) (append '(#\t #\\) acc))]
                [(char=? c #\return) (loop (+ i 1) (append '(#\r #\\) acc))]
                [else (loop (+ i 1) (cons c acc))]))))))

(define (json-string s port)
  (display "\"" port)
  (display (json-escape s) port)
  (display "\"" port))

(define (json-string-list lst port)
  (display "[" port)
  (let loop ([l lst] [first? #t])
    (unless (null? l)
      (unless first? (display ", " port))
      (json-string (format "~a" (car l)) port)
      (loop (cdr l) #f)))
  (display "]" port))

(define (export-skill-json skill-name port)
  (let ([info (lattice-info skill-name)]
        [exports (lattice-skill-exports skill-name)])
    (if (not info)
        (begin
          (display (format "WARNING: No info for skill ~a\n" skill-name)
                   (current-error-port))
          #f)
        (let ([description (or (cdr (assq 'description info)) "")]
              [keywords (or (cdr (assq 'keywords info)) '())]
              [aliases (or (cdr (assq 'aliases info)) '())]
              [deps (or (cdr (assq 'dependencies info)) '())])
          (display "  {\n" port)
          (display "    \"name\": " port)
          (json-string (symbol->string skill-name) port)
          (display ",\n    \"description\": " port)
          (json-string description port)
          (display ",\n    \"keywords\": " port)
          (json-string-list (map symbol->string keywords) port)
          (display ",\n    \"aliases\": " port)
          (json-string-list (map symbol->string aliases) port)
          (display ",\n    \"dependencies\": " port)
          (json-string-list (map symbol->string deps) port)
          (display ",\n    \"exports\": " port)
          (json-string-list (map symbol->string (or exports '())) port)
          (display "\n  }" port)
          #t))))

(define (export-all output-file)
  ;; Initialize lattice
  (display "Initializing lattice...\n")
  (lattice-init!)

  (let ([skills (kg-skills)]
        [stats (lattice-stats)])
    (display (format "Found ~a skills\n" (length skills)))

    (call-with-output-file output-file
      (lambda (port)
        (display "[\n" port)
        (let loop ([s skills] [first? #t] [exported 0])
          (if (null? s)
              (begin
                (display "\n]\n" port)
                (display (format "Exported ~a skills to ~a\n" exported output-file)))
              (begin
                (unless first? (display ",\n" port))
                (let ([ok (export-skill-json (car s) port)])
                  (loop (cdr s) #f (if ok (+ exported 1) exported))))))))

    ;; Print summary
    (display "\nLattice stats:\n")
    (for-each
      (lambda (stat)
        (display (format "  ~a: ~a\n" (car stat) (cdr stat))))
      stats)))

(export-all "user/rlm/curriculum/lattice-meta.json")
