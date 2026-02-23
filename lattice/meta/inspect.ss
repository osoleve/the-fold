(unless (top-level-bound? 'kg-skills) (load "lattice/meta/kg.ss"))
(unless (top-level-bound? 'lattice-deps) (load "lattice/meta/dag.ss"))

(doc 'module 'inspect)
(doc 'description "Skill introspection providing detailed information for agent consumption")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'note "I/O functions (test discovery, export verification) in boundary/meta/inspect-io.ss")

;;; ====
;;; Skill Description
;;; ====

;;; lattice-describe : Symbol -> void
;;; Agent-compact skill description: 1-line summary, metadata, modules with require paths
(define (lattice-describe skill-name)
  (let ([info (lattice-info skill-name)])
       (if (not info)
           (if (and (top-level-bound? 'module-name->path)
                    (module-name->path skill-name))
               (printf "~a is a module, not a skill. Use (require '~a) to load it, or (lf \"~a\") to find its parent skill.\n"
                       skill-name skill-name skill-name)
               (printf "Skill not found: ~a\n" skill-name))
           (let ([name (cdr (assq 'name info))]
                 [version (cdr (assq 'version info))]
                 [tier (cdr (assq 'tier info))]
                 [purity (cdr (assq 'purity info))]
                 [stability (cdr (assq 'stability info))]
                 [desc (cdr (assq 'description info))]
                 [deps (cdr (assq 'dependencies info))]
                 [uses (cdr (assq 'dependents info))]
                 [mod-count (cdr (assq 'module-count info))]
                 [exp-count (cdr (assq 'export-count info))]
                 [modules (kg-modules skill-name)])
                (printf "~a v~a — ~a\n" name version
                        (truncate-string (first-line desc) 60))
                (printf "tier ~a | ~a | ~a | ~a modules, ~a exports\n"
                        tier purity stability mod-count exp-count)
                (printf "deps: ~a | used-by: ~a\n"
                        (if (null? deps) "(none)" deps)
                        (if (null? uses) "(none)" uses))
                (unless (null? modules)
                  (printf "\n")
                  (for-each
                   (lambda (mod)
                     (let* ([mod-name (car mod)]
                            [bare-name (module-bare-name mod-name)])
                       (printf "  ~a  (require '~a)\n" mod-name bare-name)))
                   modules))))))

;;; lattice-describe-pretty : Symbol -> void
;;; Verbose human-oriented skill description
(define (lattice-describe-pretty skill-name)
  (if (not (kg-initialized?))
      (begin
        (printf "Knowledge graph not initialized.\n")
        (printf "Run (lattice-init!) first.\n"))
      (let ([data (kg-skill-data skill-name)])
           (if (not data)
               (printf "Skill not found: ~a\n" skill-name)
           (let ([name (cdr (assq 'name data))]
                 [version (cdr (or (assq 'version data) '(version . "0.0.0")))]
                 [tier (cdr (or (assq 'tier data) '(tier . 0)))]
                 [path (cdr (or (assq 'path data) '(path . "")))]
                 [purity (cdr (or (assq 'purity data) '(purity . unknown)))]
                 [stability (cdr (or (assq 'stability data) '(stability . experimental)))]
                 [fuel-bound (cdr (or (assq 'fuel-bound data) '(fuel-bound . "O(?)")))]
                 [description (cdr (or (assq 'description data) '(description . "")))]
                 [keywords (cdr (or (assq 'keywords data) '(keywords . ())))]
                 [aliases (cdr (or (assq 'aliases data) '(aliases . ())))]
                 [deps (kg-deps skill-name)]
                 [uses (kg-uses skill-name)]
                 [modules (kg-modules skill-name)])

                (printf "~a\n" (make-string 60 #\=))
                (printf "~a v~a\n" name version)
                (printf "~a\n\n" (make-string 60 #\=))

                (when (and description (not (string=? description "")))
                      (printf "~a\n\n" (string-trim description)))

                (printf "Tier:      ~a\n" tier)
                (printf "Purity:    ~a\n" purity)
                (printf "Stability: ~a\n" stability)
                (printf "Fuel:      ~a\n" fuel-bound)
                (printf "Path:      ~a\n\n" path)

                (unless (null? keywords)
                        (printf "Keywords:  ~a\n" keywords))
                (unless (null? aliases)
                        (printf "Aliases:   ~a\n" aliases))

                (printf "\nDependencies: ")
                (if (null? deps)
                    (printf "(none)\n")
                    (printf "~a\n" deps))

                (printf "Used by:      ")
                (if (null? uses)
                    (printf "(none)\n")
                    (printf "~a\n" uses))

                (printf "\nModules (~a):\n" (length modules))
                (for-each
                 (lambda (mod)
                   (let* ([mod-name (car mod)]
                          [bare-name (module-bare-name mod-name)])
                     (printf "  - ~a  (require '~a)\n" mod-name bare-name)))
                 modules))))))

;;; first-line : String -> String
;;; Extract first non-empty line from a (possibly multiline) string
(define (first-line str)
  (if (not (string? str))
      ""
      (let ([len (string-length str)])
           (let loop ([i 0])
                (cond
                 [(>= i len) (string-trim str)]
                 [(char=? (string-ref str i) #\newline)
                  (let ([line (string-trim (substring str 0 i))])
                       (if (string=? line "")
                           ;; skip blank leading line, try next
                           (first-line (substring str (+ i 1) len))
                           line))]
                 [else (loop (+ i 1))])))))

;;; module-bare-name : Symbol -> String
;;; Extract bare module name from potentially namespaced key (linalg/vec -> vec)
(define (module-bare-name mod-name)
  (let* ([name-str (symbol->string mod-name)]
         [slash-pos (let loop ([i 0])
                      (cond
                        [(>= i (string-length name-str)) #f]
                        [(char=? (string-ref name-str i) #\/) i]
                        [else (loop (+ i 1))]))])
    (if slash-pos
        (substring name-str (+ slash-pos 1) (string-length name-str))
        name-str)))

;;; string-trim : String -> String
;;; Remove leading/trailing whitespace and collapse internal whitespace
(define (string-trim str)
  (if (not (string? str))
      ""
      (let* ([chars (string->list str)]
             [trimmed (trim-whitespace chars)])
            (list->string trimmed))))

(define (trim-whitespace chars)
  ;; Remove leading whitespace
  (let ([chars (let loop ([cs chars])
                    (if (and (pair? cs) (char-whitespace? (car cs)))
                        (loop (cdr cs))
                        cs))])
       ;; Remove trailing whitespace
       (reverse
        (let loop ([cs (reverse chars)])
             (if (and (pair? cs) (char-whitespace? (car cs)))
                 (loop (cdr cs))
                 cs)))))

;;; ====
;;; Export Listing
;;; ====

;;; lattice-skill-exports : Symbol -> (List Symbol)
;;; Get all exports from a skill.
;;; Handles two manifest formats:
;;;   Grouped: (exports (module sym1 sym2) (module sym3 sym4))
;;;   Flat:    (exports sym1 sym2 sym3)
(define (lattice-skill-exports skill-name)
  (let* ([data (kg-skill-data skill-name)]
         [exports-raw (if data
                          (cdr (or (assq 'exports data) '(exports . ())))
                          '())])
    (cond
      [(not (list? exports-raw)) '()]
      [(null? exports-raw) '()]
      ;; Flat format: bare symbols directly under exports
      [(symbol? (car exports-raw))
       (filter symbol? exports-raw)]
      ;; Grouped format: (module-name sym1 sym2 ...) per group
      [else
       (append-map (lambda (group)
                     (cond
                       [(and (pair? group) (list? group))
                        (cdr group)]  ; Skip module name
                       [(symbol? group) (list group)]  ; Stray bare symbol
                       [else '()]))
                   exports-raw)])))

;;; lattice-all-exports : -> (List (skill . (exports ...)))
;;; Get all exports grouped by skill
(define (lattice-all-exports)
  (map (lambda (skill-name)
               (cons skill-name (lattice-skill-exports skill-name)))
       (kg-skills)))

;;; lattice-exports-compact : Symbol -> void
;;; Agent-compact exports: grouped by module with require paths, truncated per group.
;;; For flat exports, uses *export-module-map* to reconstruct grouping.
(define (lattice-exports-compact skill-name)
  (let ([data (kg-skill-data skill-name)])
    (if (not data)
        (if (and (top-level-bound? 'module-name->path)
                 (module-name->path skill-name))
            (printf "~a is a module, not a skill. Use (require '~a) to load it, or (lf \"~a\") to find its parent skill.\n"
                    skill-name skill-name skill-name)
            (printf "Skill not found: ~a\n" skill-name))
        (let* ([exports-raw (cdr (or (assq 'exports data) '(exports . ())))]
               [total (length (lattice-skill-exports skill-name))])
          (printf "~a — ~a exports\n\n" skill-name total)
          (cond
            [(or (not (list? exports-raw)) (null? exports-raw))
             (printf "  (none)\n")]
            ;; Already grouped: (module-name sym1 sym2 ...) ...
            [(and (pair? (car exports-raw)) (list? (car exports-raw)))
             (for-each
              (lambda (group)
                (when (and (pair? group) (list? group))
                  (let* ([mod-name (car group)]
                         [syms (filter symbol? (cdr group))]
                         [n (length syms)]
                         [max-show 20])
                    (printf "~a (require '~a): ~a exports\n" mod-name mod-name n)
                    (for-each (lambda (sym) (printf "  ~a\n" sym))
                              (take-at-most max-show syms))
                    (when (> n max-show)
                      (printf "  ...and ~a more\n" (- n max-show)))
                    (printf "\n"))))
              exports-raw)]
            ;; Flat: group via *export-module-map* where possible
            [else
             (let* ([syms (lattice-skill-exports skill-name)]
                    [grouped (group-exports-by-module syms)])
               (for-each
                (lambda (group)
                  (let* ([mod-name (car group)]
                         [mod-syms (cdr group)]
                         [n (length mod-syms)]
                         [max-show 20])
                    (if mod-name
                        (printf "~a (require '~a): ~a exports\n" mod-name mod-name n)
                        (printf "(ungrouped): ~a exports\n" n))
                    (for-each (lambda (sym) (printf "  ~a\n" sym))
                              (take-at-most max-show mod-syms))
                    (when (> n max-show)
                      (printf "  ...and ~a more\n" (- n max-show)))
                    (printf "\n")))
                grouped))])))))

;;; group-exports-by-module : (List Symbol) -> (List (module-or-#f . (syms ...)))
;;; Group exports using *export-module-map*. Ungrouped exports get #f key (sorted last).
(define (group-exports-by-module syms)
  (let ([groups '()])
    (for-each
     (lambda (sym)
       (let* ([mod (hashtable-ref *export-module-map* sym #f)]
              [existing (assq mod groups)])
         (if existing
             (set-cdr! existing (cons sym (cdr existing)))
             (set! groups (cons (list mod sym) groups)))))
     syms)
    ;; Reverse each group's syms, put #f (ungrouped) last
    (let* ([finalized (map (lambda (g) (cons (car g) (reverse (cdr g))))
                           (reverse groups))]
           [with-mod (filter (lambda (g) (car g)) finalized)]
           [without-mod (filter (lambda (g) (not (car g))) finalized)])
      (append with-mod without-mod))))

;;; lattice-exports-pretty : Symbol -> void
;;; Verbose human-oriented exports listing
(define (lattice-exports-pretty skill-name)
  (let ([data (kg-skill-data skill-name)])
    (if (not data)
        (printf "Skill not found: ~a\n" skill-name)
        (let ([exports-raw (cdr (or (assq 'exports data) '(exports . ())))])
          (printf "Exports for ~a\n" skill-name)
          (printf "~a\n\n" (make-string 40 #\-))
          (cond
            [(not (list? exports-raw)) (void)]
            [(null? exports-raw) (printf "  (none)\n")]
            ;; Flat format: bare symbols
            [(symbol? (car exports-raw))
             (for-each (lambda (exp)
                         (when (symbol? exp)
                           (printf "  ~a\n" exp)))
                       exports-raw)
             (printf "\n")]
            ;; Grouped format
            [else
             (for-each
              (lambda (group)
                (when (and (pair? group) (list? group))
                  (printf "~a:\n" (car group))
                  (for-each
                   (lambda (exp) (printf "  ~a\n" exp))
                   (cdr group))
                  (printf "\n")))
              exports-raw)])))))

;;; ====
;;; Module Listing
;;; ====

;;; lattice-modules-detail : Symbol -> void
;;; Pretty-print module details for a skill
(define (lattice-modules-detail skill-name)
  (let ([data (kg-skill-data skill-name)])
       (if (not data)
           (printf "Skill not found: ~a\n" skill-name)
           (let ([modules-raw (cdr (or (assq 'modules data) '(modules . ())))])
                (printf "Modules for ~a\n" skill-name)
                (printf "~a\n\n" (make-string 40 #\-))
                (for-each
                 (lambda (mod)
                         (when (and (pair? mod) (>= (length mod) 3))
                               (let ([name (car mod)]
                                     [file (cadr mod)]
                                     [desc (caddr mod)])
                                    (printf "~a\n" name)
                                    (printf "  File: ~a\n" file)
                                    (printf "  ~a\n\n" desc))))
                 (if (list? modules-raw) modules-raw '()))))))

;;; ====
;;; Source Location
;;; ====

;;; lattice-source : Symbol -> String | #f
;;; Get source file for an export (if discoverable)
(define (lattice-source export-name)
  (let loop ([skills (kg-skills)])
       (if (null? skills)
           #f
           (let* ([skill-name (car skills)]
                  [data (kg-skill-data skill-name)]
                  [path (if data (cdr (or (assq 'path data) '(path . ""))) "")]
                  [exports-raw (if data
                                   (cdr (or (assq 'exports data) '(exports . ())))
                                   '())])
                 ;; Search through export groups
                 (let find-in-groups ([groups exports-raw])
                      (if (null? groups)
                          (loop (cdr skills))  ; Not in this skill, try next
                          (let ([group (car groups)])
                               (if (and (pair? group) (list? group))
                                   (if (memq export-name (cdr group))
                                       ;; Found it! Get module file
                                       (let* ([mod-name (car group)]
                                              [modules-raw (cdr (or (assq 'modules data) '(modules . ())))]
                                              [mod-entry (find (lambda (m)
                                                                       (and (pair? m)
                                                                            (eq? (car m) mod-name)))
                                                               modules-raw)])
                                             (if (and mod-entry (>= (length mod-entry) 2))
                                                 (string-append path "/" (cadr mod-entry))
                                                 path))
                                       (find-in-groups (cdr groups)))
                                   (find-in-groups (cdr groups))))))))))

;;; find helper
(define (find pred lst)
  (cond
   [(null? lst) #f]
   [(pred (car lst)) (car lst)]
   [else (find pred (cdr lst))]))

;;; ====
;;; Agent-Friendly Info
;;; ====

;;; lattice-info : Symbol -> Alist | #f
;;; Get structured info about a skill for agent consumption
(define (lattice-info skill-name)
  (let ([data (kg-skill-data skill-name)])
       (if (not data)
           #f
           (let ([deps (kg-deps skill-name)]
                 [uses (kg-uses skill-name)]
                 [modules (kg-modules skill-name)]
                 [exports (lattice-skill-exports skill-name)])
                `((name . ,skill-name)
                  (version . ,(cdr (or (assq 'version data) '(version . "0.0.0"))))
                  (tier . ,(cdr (or (assq 'tier data) '(tier . 0))))
                  (purity . ,(cdr (or (assq 'purity data) '(purity . unknown))))
                  (stability . ,(cdr (or (assq 'stability data) '(stability . experimental))))
                  (path . ,(cdr (or (assq 'path data) '(path . ""))))
                  (description . ,(cdr (or (assq 'description data) '(description . ""))))
                  (dependencies . ,deps)
                  (dependents . ,uses)
                  (module-count . ,(length modules))
                  (export-count . ,(length exports))
                  (keywords . ,(cdr (or (assq 'keywords data) '(keywords . ()))))
                  (aliases . ,(cdr (or (assq 'aliases data) '(aliases . ())))))))))

;;; lattice-summary : -> void
;;; Print one-line summary for each skill
(define (lattice-summary)
  (if (not (kg-initialized?))
      (begin
        (printf "Knowledge graph not initialized.\n")
        (printf "Run (lattice-init!) first.\n"))
      (begin
        (printf "~20a ~8a ~8a ~6a ~a\n" "Skill" "Tier" "Purity" "Mods" "Description")
        (printf "~20a ~8a ~8a ~6a ~a\n" "----" "----" "----" "----" "----")
        (for-each
         (lambda (skill-name)
                 (let ([info (lattice-info skill-name)])
                      (when info
                            (printf "~20a ~8a ~8a ~6a ~a\n"
                                    (cdr (assq 'name info))
                                    (cdr (assq 'tier info))
                                    (cdr (assq 'purity info))
                                    (cdr (assq 'module-count info))
                                    (truncate-string (cdr (assq 'description info)) 40)))))
         (kg-skills)))))

;;; truncate-string : String Int -> String
(define (truncate-string str max-len)
  (let ([s (string-trim str)])
       (if (<= (string-length s) max-len)
           s
           (string-append (substring s 0 (- max-len 3)) "..."))))

;;; ====
;;; Structured Data API (for MCP/IPC)
;;; ====

(doc lattice-describe-data 'type (-> Symbol (Maybe Alist)))
(doc lattice-describe-data 'description "Structured skill info for MCP/IPC. Returns enriched alist or #f if not found.")
;;; lattice-describe-data : Symbol -> Alist | #f
;;; Returns the lattice-info alist enriched with module require paths.
;;; Callers get a single self-contained S-expression per skill.
(define (lattice-describe-data skill-name)
  (let ([info (lattice-info skill-name)])
    (if (not info)
        #f
        (let ([modules (kg-modules skill-name)])
          `(,@info
            (modules . ,(map (lambda (mod)
                               (let* ([mod-name (car mod)]
                                      [bare (module-bare-name mod-name)])
                                 `((name . ,mod-name)
                                   (require . ,bare))))
                             modules)))))))

(doc lattice-exports-data 'type (-> Symbol (Maybe Alist)))
(doc lattice-exports-data 'description "Structured exports for MCP/IPC. Returns alist with grouped exports or #f if not found.")
;;; lattice-exports-data : Symbol -> Alist | #f
;;; Returns: ((skill . name) (total . N) (groups ((name . mod) (require . mod) (exports sym1 sym2 ...)) ...))
(define (lattice-exports-data skill-name)
  (let ([data (kg-skill-data skill-name)])
    (if (not data)
        #f
        (let* ([exports-raw (cdr (or (assq 'exports data) '(exports . ())))]
               [total (length (lattice-skill-exports skill-name))])
          `((skill . ,skill-name)
            (total . ,total)
            (groups . ,(exports-raw->groups exports-raw skill-name)))))))

;;; exports-raw->groups : List Symbol -> (List Alist)
;;; Convert raw manifest exports into structured module groups.
;;; Handles both grouped ((mod sym1 sym2) ...) and flat (sym1 sym2 ...) formats.
(define (exports-raw->groups exports-raw skill-name)
  (cond
    [(or (not (list? exports-raw)) (null? exports-raw))
     '()]
    ;; Already grouped: ((module-name sym1 sym2 ...) ...)
    [(and (pair? (car exports-raw)) (list? (car exports-raw)))
     (filter-map
      (lambda (group)
        (and (pair? group) (list? group)
             (let* ([mod-name (car group)]
                    [syms (filter symbol? (cdr group))])
               `((name . ,mod-name)
                 (require . ,(symbol->string mod-name))
                 (exports . ,syms)))))
      exports-raw)]
    ;; Flat: group via *export-module-map* where possible
    [else
     (let* ([syms (lattice-skill-exports skill-name)]
            [grouped (group-exports-by-module syms)])
       (map (lambda (group)
              (let ([mod-name (car group)]
                    [mod-syms (cdr group)])
                `((name . ,(or mod-name 'ungrouped))
                  ,@(if mod-name
                        `((require . ,(symbol->string mod-name)))
                        '())
                  (exports . ,mod-syms))))
            grouped))]))

;;; ====
;;; Convenience Functions (for REPL)
;;; ====

;;; li : Symbol -> void
;;; Inspect skill (agent-compact)
(define (li skill-name)
  (lattice-describe skill-name))

;;; le : Symbol -> void
;;; List exports (agent-compact, grouped by module)
(define (le skill-name)
  (lattice-exports-compact skill-name))

;;; lm : Symbol -> void
;;; List modules with details
(define (lm skill-name)
  (lattice-modules-detail skill-name))

;;; li-pretty : Symbol -> void
;;; Inspect skill (verbose)
(define (li-pretty skill-name)
  (lattice-describe-pretty skill-name))

;;; le-pretty : Symbol -> void
;;; List exports (verbose, ungrouped)
(define (le-pretty skill-name)
  (lattice-exports-pretty skill-name))

;;; ====
;;; Test Result Parsing (pure — operates on output strings)
;;; ====

;;; parse-test-result-line : String -> Alist | #f
;;; Parse the structured result line: [TEST-RESULT total=N passed=N failed=N]
;;; Returns alist with 'total, 'passed, 'failed or #f if not found
;;; Uses string-last-index-of to find the LAST result line (in case tests
;;; print debug output containing result lines, or run nested tests)
(define (parse-test-result-line output)
  (let ([start (string-last-index-of output "[TEST-RESULT ")])
       (if (not start)
           #f
           (let* ([end (string-index-of-after output "]" start)]
                  [line (if end
                            (substring output start (+ end 1))
                            #f)])
                 (and line
                      (let ([total (parse-kv line "total=")]
                            [passed (parse-kv line "passed=")]
                            [failed (parse-kv line "failed=")])
                           (and total passed failed
                                `((total . ,total)
                                  (passed . ,passed)
                                  (failed . ,failed)))))))))

;;; parse-kv : String String -> Integer | #f
;;; Extract integer value after key= from string
(define (parse-kv str key)
  (let ([pos (string-index-of str key)])
       (if (not pos)
           #f
           (let* ([start (+ pos (string-length key))]
                  [rest (substring str start (string-length str))]
                  [digits (extract-leading-digits rest)])
                 (and (not (string=? digits ""))
                      (string->number digits))))))

;;; extract-leading-digits : String -> String
;;; Extract leading digit characters from string
(define (extract-leading-digits str)
  (let loop ([i 0] [acc '()])
       (if (>= i (string-length str))
           (list->string (reverse acc))
           (let ([c (string-ref str i)])
                (if (char-numeric? c)
                    (loop (+ i 1) (cons c acc))
                    (list->string (reverse acc)))))))

;;; string-index-of-after : String String Integer -> Integer | #f
;;; Find substring in string starting search at given position
(define (string-index-of-after haystack needle start)
  (let* ([rest (substring haystack start (string-length haystack))]
         [idx (string-index-of rest needle)])
        (and idx (+ start idx))))

;;; truncate-error-output : String -> String
;;; Truncate error output to last N characters for readability
(define (truncate-error-output output)
  (let ([max-len 500])
       (if (<= (string-length output) max-len)
           output
           (string-append "...\n" (substring output
                                             (- (string-length output) max-len)
                                             (string-length output))))))

;;; ====
;;; REPL Interface
;;; ====

(unless (top-level-bound? '*inspect-banner-shown*)
  (meta-printf "inspect.ss loaded.\n")
  (meta-printf "  (li 'skill)                   - Inspect (agent-compact)\n")
  (meta-printf "  (le 'skill)                   - Exports (grouped by module)\n")
  (meta-printf "  (lm 'skill)                   - Module details\n")
  (meta-printf "  (li-pretty 'skill)            - Inspect (verbose)\n")
  (meta-printf "  (le-pretty 'skill)            - Exports (verbose)\n")
  (meta-printf "  (lattice-info 'skill)         - Structured alist\n")
  (meta-printf "  (lattice-source 'export)      - Source file location\n")
  (meta-printf "  (lattice-summary)             - All skills summary\n"))
(set-top-level-value! '*inspect-banner-shown* #t)
