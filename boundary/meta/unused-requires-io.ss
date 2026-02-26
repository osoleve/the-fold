;;; boundary/meta/unused-requires-io.ss — I/O orchestrator for unused-require detection
;;; @module unused-requires-io
;;; @requires file-io exports-io meta/unused-requires

(unless (top-level-bound? 'read-all-sexps) (load "boundary/meta/file-io.ss"))
(unless (top-level-bound? 'extract-all-exports-from-file) (load "boundary/meta/exports-io.ss"))
(unless (top-level-bound? 'analyze-file-requires) (load "lattice/meta/unused-requires.ss"))

(doc 'module 'unused-requires-io)
(doc 'description "I/O layer for unused-require detection. Builds module export indices
from source scanning and manifests, then runs pure analysis from
lattice/meta/unused-requires.ss against actual files.")
(doc 'layer 'boundary)

;;; ====
;;; Module Export Index
;;; ====

(doc 'section 'export-index)

;;; Global export index: module-name -> (list-of-exported-symbols)
(define *module-exports-index* '())

;;; Core modules that predate the doc system — explicit name→path mapping.
;;; These are the most commonly required across the lattice.
(define *core-module-paths*
  '((prelude   . "core/base/prelude.ss")
    (sha256    . "core/base/sha256.ss")
    (error     . "core/base/error.ss")
    (block     . "core/blocks/block.ss")
    (cas       . "core/blocks/cas.ss")
    (normalize . "core/blocks/normalize.ss")
    (parse     . "core/lang/parse.ss")
    (prim      . "core/lang/prim.ss")
    (eval      . "core/lang/eval.ss")
    (compile   . "core/lang/compile.ss")
    (types     . "core/types/types.ss")
    (kinds     . "core/types/kinds.ss")
    (infer     . "core/types/infer.ss")
    (resolve   . "core/types/resolve.ss")
    (dep-types . "core/types/dep-types.ss")))

;;; build-module-exports-index! : -> Void
;;; Build a mapping from module names to their exported symbols.
;;; Strategy (in priority order):
;;;   1. Manifest exports (grouped by module) — curated, most complete
;;;   2. All top-level defines from source files — fallback for modules
;;;      not in manifests or with empty manifest entries
(define (build-module-exports-index!)
  (unless (top-level-bound? 'parse-manifest) (load "lattice/meta/manifest.ss"))
  (let ([manifest-paths (find-manifests "lattice")]
        [index '()])
    ;; Phase 1: Extract from manifests (grouped exports)
    (for-each
     (lambda (mpath)
       (let ([manifest-sexp (read-manifest-sexp mpath)])
         (when manifest-sexp
           (let* ([parsed (parse-manifest manifest-sexp)]
                  [exports-raw (cdr (assq 'exports parsed))])
             ;; exports-raw is either flat symbols or grouped: (module-name sym1 sym2 ...)
             (for-each
              (lambda (item)
                (when (and (pair? item) (symbol? (car item)))
                  ;; Grouped export: (module-name sym1 sym2 ...)
                  (let ([mod-name (car item)]
                        [syms (filter symbol? (cdr item))])
                    (when (pair? syms)
                      (set! index (cons (cons mod-name syms) index))))))
              exports-raw)))))
     manifest-paths)

    ;; Phase 2: Index core modules via top-level defines.
    ;; Core modules aren't in lattice manifests but are widely required.
    ;; These predate the doc system, so we map names to paths explicitly.
    (for-each
     (lambda (entry)
       (let ([mod-name (car entry)]
             [path (cdr entry)])
         (guard (e [else #f])
           (when (file-exists? path)
             (let ([defines (extract-top-level-defines path)])
               (when (pair? defines)
                 ;; Override any stale lattice re-export group
                 (set! index (cons (cons mod-name defines)
                                   (remp (lambda (e) (eq? (car e) mod-name))
                                         index)))))))))
     *core-module-paths*)

    ;; Phase 3: Fill gaps — scan source files for modules not yet in index
    (for-each
     (lambda (mpath)
       (let ([skill-dir (path-dirname mpath)])
         (guard (e [else #f])
           (let ([files (find-scheme-files-ex skill-dir)])
             (for-each
              (lambda (path)
                (let ([info (extract-module-info path)])
                  (when info
                    (let ([mod-name (car info)])
                      ;; Only fill if not already in index from manifest or core
                      (unless (assq mod-name index)
                        (let ([defines (extract-top-level-defines path)])
                          (when (pair? defines)
                            (set! index (cons (cons mod-name defines) index)))))))))
              files)))))
     manifest-paths)

    (set! *module-exports-index* index)
    (length index)))

;;; extract-top-level-defines : String -> (List Symbol)
;;; Extract all top-level define names from a file (fallback when no
;;; manifest data or doc annotations). This is conservative — includes
;;; everything, which means we'll never falsely report "unused" for a
;;; module that IS used but whose exports we don't know.
(define (extract-top-level-defines path)
  (guard (e [else '()])
    (let ([sexps (read-all-sexps path)])
      (filter-map
       (lambda (sexp)
         (and (pair? sexp)
              (eq? (car sexp) 'define)
              (pair? (cdr sexp))
              (let ([name-part (cadr sexp)])
                (cond
                  [(symbol? name-part) name-part]
                  [(and (pair? name-part) (symbol? (car name-part)))
                   (car name-part)]
                  [else #f]))))
       sexps))))

;;; module-exports-lookup : Symbol -> (List Symbol)
;;; Look up exports for a module from the index.
;;; Returns '() if module not found (will be classified as 'uncertain).
(define (module-exports-lookup mod)
  (let ([entry (assq mod *module-exports-index*)])
    (if entry (cdr entry) '())))

;;; path-dirname : String -> String
;;; Get directory part of a path.
(define (path-dirname path)
  (let loop ([i (- (string-length path) 1)])
    (cond
      [(< i 0) "."]
      [(char=? (string-ref path i) #\/) (substring path 0 i)]
      [else (loop (- i 1))])))

;;; ====
;;; File Analysis
;;; ====

(doc 'section 'file-analysis)

;;; unused-requires-file : String -> RequireAnalysis
;;; Analyze a single file for unused requires.
;;; Returns: ((unused . (mods)) (used . (mods)) (uncertain . (mods)))
(define (unused-requires-file path)
  (guard (e [else
             (fprintf (current-error-port) "Warning: error analyzing ~a: ~a\n" path
                      (if (message-condition? e) (condition-message e) e))
             '((unused . ()) (used . ()) (uncertain . ()))])
    (let ([sexps (read-all-sexps path)])
      (if (null? sexps)
          '((unused . ()) (used . ()) (uncertain . ()))
          (analyze-file-requires sexps module-exports-lookup)))))

;;; unused-requires-file-detail : String -> DetailedAnalysis
;;; Analyze a single file with per-module export usage detail.
(define (unused-requires-file-detail path)
  (let ([sexps (read-all-sexps path)])
    (if (null? sexps)
        '()
        (analyze-file-requires-detail sexps module-exports-lookup))))

;;; ====
;;; Skill Analysis
;;; ====

(doc 'section 'skill-analysis)

;;; unused-requires-skill : String -> SkillReport
;;; Analyze all non-test .ss files in a skill directory.
;;; Returns: ((skill . name) (files . ((path . analysis) ...)))
(define (unused-requires-skill skill-dir)
  (let* ([files (find-scheme-files-ex skill-dir)]
         [results
          (filter-map
           (lambda (path)
             (let ([result (unused-requires-file path)])
               ;; Only include files that have requires
               (if (and (null? (analysis-unused result))
                        (null? (analysis-used result))
                        (null? (analysis-uncertain result)))
                   #f
                   (cons path result))))
           files)])
    `((skill . ,(path-basename skill-dir))
      (files . ,results))))

;;; path-basename : String -> String
(define (path-basename path)
  (let loop ([i (- (string-length path) 1)])
    (cond
      [(< i 0) path]
      [(char=? (string-ref path i) #\/)
       (substring path (+ i 1) (string-length path))]
      [else (loop (- i 1))])))

;;; ====
;;; Full Lattice Scan
;;; ====

(doc 'section 'full-scan)

;;; unused-requires-all : -> FullReport
;;; Scan the entire lattice for unused requires.
;;; Returns: ((skill-name . ((path . analysis) ...)) ...)
(define (unused-requires-all)
  (let ([manifest-paths (find-manifests "lattice")])
    (filter-map
     (lambda (mpath)
       (let* ([skill-dir (path-dirname mpath)]
              [report (unused-requires-skill skill-dir)]
              [files (cdr (assq 'files report))]
              ;; Only include skills that have at least one unused require
              [has-unused (exists (lambda (f)
                                   (pair? (analysis-unused (cdr f))))
                                 files)])
         (if has-unused
             (cons (cdr (assq 'skill report)) files)
             #f)))
     manifest-paths)))

;;; ====
;;; Pretty Printing
;;; ====

(doc 'section 'display)

;;; print-unused-requires : FullReport -> Void
;;; Pretty-print the results of unused-requires-all.
(define (print-unused-requires report)
  (if (null? report)
      (printf "No unused requires found across the lattice.\n")
      (begin
        (printf "Unused requires found:\n")
        (printf "~a\n" (make-string 60 #\=))
        (for-each
         (lambda (skill-entry)
           (let ([skill (car skill-entry)]
                 [files (cdr skill-entry)])
             (printf "\n~a/\n" skill)
             (for-each
              (lambda (file-entry)
                (let* ([path (car file-entry)]
                       [result (cdr file-entry)]
                       [unused (analysis-unused result)])
                  (when (pair? unused)
                    (printf "  ~a\n" (path-basename path))
                    (for-each
                     (lambda (mod) (printf "    - (require '~a)  [UNUSED]\n" mod))
                     unused))))
              files)))
         report)
        (printf "\n~a\n" (make-string 60 #\=))
        ;; Summary
        (let ([total-files (apply + (map (lambda (s)
                                           (length (filter
                                                    (lambda (f)
                                                      (pair? (analysis-unused (cdr f))))
                                                    (cdr s))))
                                         report))]
              [total-unused (apply + (map (lambda (s)
                                            (apply + (map (lambda (f)
                                                            (length (analysis-unused (cdr f))))
                                                          (cdr s))))
                                         report))])
          (printf "~a unused requires across ~a files in ~a skills\n"
                  total-unused total-files (length report))))))

;;; print-file-detail : String -> Void
;;; Print detailed require usage for a single file.
(define (print-file-detail path)
  (let ([detail (unused-requires-file-detail path)])
    (if (null? detail)
        (printf "No requires in ~a\n" path)
        (begin
          (printf "Require analysis for ~a:\n\n" path)
          (for-each
           (lambda (entry)
             (let* ([mod (car entry)]
                    [info (cdr entry)]
                    [status (cdr (assq 'status info))]
                    [used (cdr (assq 'exports-used info))]
                    [total (cdr (assq 'exports-total info))])
               (case status
                 [(used)
                  (printf "  ~a  [USED ~a/~a exports]\n" mod (length used) total)
                  (for-each (lambda (e) (printf "    + ~a\n" e)) used)]
                 [(unused)
                  (printf "  ~a  [UNUSED - 0/~a exports referenced]\n" mod total)]
                 [(uncertain)
                  (printf "  ~a  [UNCERTAIN - no export data]\n" mod)])))
           detail)))))

;;; ====
;;; Convenience: ensure index is built
;;; ====

(define *unused-requires-initialized* #f)

;;; ensure-exports-index! : -> Void
;;; Build the export index if not already done.
(define (ensure-exports-index!)
  (unless *unused-requires-initialized*
    (printf "Building module exports index...")
    (let ([count (build-module-exports-index!)])
      (printf " ~a modules indexed.\n" count))
    (set! *unused-requires-initialized* #t)))

;;; ====
;;; Top-Level API
;;; ====

(doc 'section 'api)

;;; ur-file : String -> Void
;;; Quick: show unused requires for a file.
(define (ur-file path)
  (ensure-exports-index!)
  (let* ([result (unused-requires-file path)]
         [unused (analysis-unused result)])
    (if (null? unused)
        (printf "~a: all requires used\n" path)
        (begin
          (printf "~a: ~a unused require~a\n"
                  path (length unused) (if (= 1 (length unused)) "" "s"))
          (for-each (lambda (m) (printf "  - ~a\n" m)) unused)))))

;;; ur-skill : String -> Void
;;; Quick: show unused requires for a skill directory.
(define (ur-skill skill-dir)
  (ensure-exports-index!)
  (let* ([report (unused-requires-skill skill-dir)]
         [files (cdr (assq 'files report))])
    (let ([files-with-unused (filter (lambda (f) (pair? (analysis-unused (cdr f)))) files)])
      (if (null? files-with-unused)
          (printf "~a: no unused requires\n" skill-dir)
          (begin
            (printf "~a:\n" skill-dir)
            (for-each
             (lambda (f)
               (let ([path (car f)]
                     [unused (analysis-unused (cdr f))])
                 (printf "  ~a: ~a\n" (path-basename path)
                         (map symbol->string unused))))
             files-with-unused))))))

;;; ur-all : -> Void
;;; Quick: scan entire lattice and print report.
(define (ur-all)
  (ensure-exports-index!)
  (print-unused-requires (unused-requires-all)))

(meta-printf "unused-requires-io.ss loaded.\n")
(meta-printf "  (ur-file \"path.ss\")       - Check single file\n")
(meta-printf "  (ur-skill \"lattice/dir\")  - Check skill directory\n")
(meta-printf "  (ur-all)                  - Scan entire lattice\n")
(meta-printf "  (print-file-detail \"p\")   - Detailed per-module usage\n")
