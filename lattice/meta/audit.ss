(doc 'module 'audit)
(doc 'description "Manifest audit tool - detects gaps between source code definitions and manifest exports")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'section 'source-file-analysis)

;;; read-file-as-string : Path -> String
(define (read-file-as-string filepath)
  (if (file-exists? filepath)
      (call-with-input-file filepath
        (lambda (port)
          (let loop ([chars '()])
               (let ([c (read-char port)])
                    (if (eof-object? c)
                        (list->string (reverse chars))
                        (loop (cons c chars)))))))
      ""))

;;; extract-defines : String -> (List Symbol)
;;; Extract all top-level (define (name ...)) from a source file
(define (extract-defines filepath)
  (let* ([content (read-file-as-string filepath)]
         [lines (string-split content #\newline)])
        (filter-map extract-define-from-line lines)))

;;; extract-define-from-line : String -> Symbol | #f
;;; Parse a line to extract define name
(define (extract-define-from-line line)
  (let ([trimmed (string-trim line)])
       (if (and (> (string-length trimmed) 9)
                (string=? (substring trimmed 0 9) "(define ("))
           (let ([rest (substring trimmed 9 (string-length trimmed))])
                (extract-symbol-name rest))
           #f)))

;;; extract-symbol-name : String -> Symbol | #f
;;; Extract symbol name from start of string
(define (extract-symbol-name str)
  (let loop ([chars (string->list str)]
             [acc '()])
       (cond
        [(null? chars) #f]
        [(or (char=? (car chars) #\space)
             (char=? (car chars) #\)))
         (if (null? acc)
             #f
             (string->symbol (list->string (reverse acc))))]
        [(valid-symbol-char? (car chars))
         (loop (cdr chars) (cons (car chars) acc))]
        [else #f])))

;;; valid-symbol-char? : Char -> Bool
(define (valid-symbol-char? c)
  (or (char-alphabetic? c)
      (char-numeric? c)
      (memv c '(#\- #\_ #\? #\! #\< #\> #\= #\+ #\* #\/))))

;;; string-split : String Char -> (List String)
(define (string-split str delim)
  (let loop ([chars (string->list str)]
             [current '()]
             [result '()])
       (cond
        [(null? chars)
         (reverse (if (null? current)
                      result
                      (cons (list->string (reverse current)) result)))]
        [(char=? (car chars) delim)
         (loop (cdr chars)
               '()
               (if (null? current)
                   result
                   (cons (list->string (reverse current)) result)))]
        [else
         (loop (cdr chars) (cons (car chars) current) result)])))

;;; string-trim : String -> String
(define (string-trim str)
  (let* ([chars (string->list str)]
         [trimmed-front (drop-while char-whitespace? chars)]
         [trimmed-back (reverse (drop-while char-whitespace? (reverse trimmed-front)))])
        (list->string trimmed-back)))

;;; drop-while : (A -> Bool) List<A> -> List<A>
(define (drop-while pred lst)
  (cond
   [(null? lst) '()]
   [(pred (car lst)) (drop-while pred (cdr lst))]
   [else lst]))

;;; ====
;;; Manifest Analysis
;;; ====

;;; parse-manifest-exports : Path -> (List Symbol)
;;; Extract exports list from a manifest file
(define (parse-manifest-exports manifest-path)
  (if (file-exists? manifest-path)
      (let ([manifest (read-manifest manifest-path)])
           (if manifest
               (extract-exports-from-manifest manifest)
               '()))
      '()))

;;; read-manifest : Path -> Sexp | #f
(define (read-manifest path)
  (guard (e [else #f])
    (with-input-from-file path read)))

;;; extract-exports-from-manifest : Sexp -> (List Symbol)
;;; Manifest is (skill name clause1 clause2 ...) where clauses are (keyword value)
(define (extract-exports-from-manifest manifest)
  (let ([clauses (cddr manifest)])  ; Skip 'skill and name
       (let ([exports-clause (assq 'exports clauses)])
            (if exports-clause
                (flatten-symbols (cdr exports-clause))
                '()))))

;;; flatten-symbols : Sexp -> (List Symbol)
;;; Flatten nested structure to list of symbols
(define (flatten-symbols x)
  (cond
   [(symbol? x) (list x)]
   [(pair? x) (append (flatten-symbols (car x))
                      (flatten-symbols (cdr x)))]
   [else '()]))

;;; ====
;;; File Discovery (using shell)
;;; ====

;;; find-source-files : Path -> (List Path)
;;; Find all .ss files (excluding tests) using shell
(define (find-source-files dir)
  (let* ([cmd (format "find ~a -name '*.ss' -type f ! -name 'test-*' 2>/dev/null" dir)]
         [result (shell-command cmd)])
        (if (and result (string? result) (> (string-length result) 0))
            (filter (lambda (s) (> (string-length s) 0))
                    (string-split result #\newline))
            '())))

;;; shell-command : String -> String | #f
;;; Execute shell command and return stdout
(define (shell-command cmd)
  (let ([tmp-file (format "/tmp/audit-~a.txt" (random 1000000))])
       (system (format "~a > ~a" cmd tmp-file))
       (if (file-exists? tmp-file)
           (let ([result (read-file-as-string tmp-file)])
                (delete-file tmp-file)
                result)
           #f)))

;;; ====
;;; Audit Functions
;;; ====

;;; audit-file : String -> AuditResult
;;; Audit a single source file
(define (audit-file filepath)
  (let ([defines (extract-defines filepath)])
       `((file . ,filepath)
         (defines . ,defines)
         (count . ,(length defines)))))

;;; audit-skill : Symbol -> AuditReport
;;; Audit a skill by comparing source definitions to manifest exports
(define (audit-skill skill-name)
  (let* ([skill-path (string-append "lattice/" (symbol->string skill-name))]
         [manifest-path (string-append skill-path "/manifest.sexp")]
         [manifest-exports (parse-manifest-exports manifest-path)]
         [source-files (find-source-files skill-path)]
         [all-defines (collect-all-defines source-files)]
         [public-defines (filter public-export? all-defines)]
         [missing (set-difference public-defines manifest-exports)]
         [extra (set-difference manifest-exports all-defines)])
        `((skill . ,skill-name)
          (manifest-exports . ,(length manifest-exports))
          (source-defines . ,(length all-defines))
          (public-defines . ,(length public-defines))
          (missing-from-manifest . ,missing)
          (missing-count . ,(length missing))
          (in-manifest-but-not-source . ,extra)
          (extra-count . ,(length extra)))))

;;; collect-all-defines : (List Path) -> (List Symbol)
(define (collect-all-defines files)
  (apply append (map extract-defines files)))

;;; public-export? : Symbol -> Bool
;;; Heuristic: public exports don't start with % or contain internal markers
(define (public-export? sym)
  (let ([name (symbol->string sym)])
       (and (> (string-length name) 0)
            (not (char=? (string-ref name 0) #\%))
            (not (string-contains? name "-internal"))
            (not (string-contains? name "-helper"))
            (not (string-contains? name "-impl")))))

;;; set-difference : (List A) (List A) -> (List A)
(define (set-difference xs ys)
  (filter (lambda (x) (not (memq x ys))) xs))

;;; string-contains? : String String -> Bool
(define (string-contains? haystack needle)
  (let ([h-len (string-length haystack)]
        [n-len (string-length needle)])
       (if (> n-len h-len)
           #f
           (let loop ([i 0])
                (cond
                 [(> (+ i n-len) h-len) #f]
                 [(string=? (substring haystack i (+ i n-len)) needle) #t]
                 [else (loop (+ i 1))])))))

;;; ====
;;; Pretty Printing
;;; ====

;;; audit-skill-pretty : Symbol -> void
(define (audit-skill-pretty skill-name)
  (let ([report (audit-skill skill-name)])
       (printf "\n====\n")
       (printf "Audit Report: ~a\n" skill-name)
       (printf "====\n\n")
       (printf "Manifest exports: ~a\n" (cdr (assq 'manifest-exports report)))
       (printf "Source defines:   ~a\n" (cdr (assq 'source-defines report)))
       (printf "Public defines:   ~a\n" (cdr (assq 'public-defines report)))
       (printf "\n")
       (let ([missing (cdr (assq 'missing-from-manifest report))]
             [extra (cdr (assq 'in-manifest-but-not-source report))])
            (if (null? missing)
                (printf "No missing exports.\n")
                (begin
                  (printf "Missing from manifest (~a):\n" (length missing))
                  (for-each (lambda (sym) (printf "  - ~a\n" sym))
                            (take-at-most 50 missing))
                  (when (> (length missing) 50)
                        (printf "  ... and ~a more\n" (- (length missing) 50)))))
            (printf "\n")
            (if (null? extra)
                (printf "No phantom exports.\n")
                (begin
                  (printf "In manifest but not found in source (~a):\n" (length extra))
                  (for-each (lambda (sym) (printf "  - ~a\n" sym))
                            (take-at-most 20 extra)))))))

;;; take-at-most : Int (List A) -> (List A)
(define (take-at-most n lst)
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (take-at-most (- n 1) (cdr lst)))))

;;; ====
;;; Export Suggestion Generator
;;; ====

;;; suggest-exports : Symbol -> Sexp
;;; Generate suggested exports clause for a skill
(define (suggest-exports skill-name)
  (let* ([skill-path (string-append "lattice/" (symbol->string skill-name))]
         [source-files (find-source-files skill-path)]
         [all-defines (collect-all-defines source-files)]
         [public-defines (filter public-export? all-defines)])
        `(exports ,public-defines)))

;;; suggest-missing : Symbol -> (List Symbol)
;;; Get just the missing exports for a skill
(define (suggest-missing skill-name)
  (let ([report (audit-skill skill-name)])
       (cdr (assq 'missing-from-manifest report))))

;;; ====
;;; Helpers
;;; ====

;;; filter-map provided by prelude

;;; ====
;;; Dependency Auditing
;;; ====

;;; extract-load-deps : String -> (List Symbol)
;;; Extract skill names from (load "lattice/X/...") statements in a file
(define (extract-load-deps filepath)
  (let* ([content (read-file-as-string filepath)]
         [lines (string-split content #\newline)])
        (filter-map extract-load-skill lines)))

;;; extract-load-skill : String -> Symbol | #f
;;; Parse (load "lattice/SKILL/...") and return SKILL symbol
(define (extract-load-skill line)
  (let ([trimmed (string-trim line)])
       (if (and (> (string-length trimmed) 16)
                (string=? (substring trimmed 0 15) "(load \"lattice/"))
           (let* ([rest (substring trimmed 15 (string-length trimmed))]
                  [slash-pos (string-index rest #\/)])
                 (if slash-pos
                     (string->symbol (substring rest 0 slash-pos))
                     #f))
           #f)))

;;; string-index : String Char -> Int | #f
(define (string-index str char)
  (let ([len (string-length str)])
       (let loop ([i 0])
            (cond
             [(>= i len) #f]
             [(char=? (string-ref str i) char) i]
             [else (loop (+ i 1))]))))

;;; parse-manifest-deps : Path -> (List Symbol)
;;; Extract declared dependencies from a manifest
;;; Manifest format: (skill name (version ...) (deps (...)) ...)
(define (parse-manifest-deps manifest-path)
  (if (file-exists? manifest-path)
      (guard (e [else '()])
             (let* ([sexp (call-with-input-file manifest-path read)]
                    ;; Skip 'skill and 'name, then search for deps
                    [body (cddr sexp)]
                    [deps-entry (assq 'deps body)])
                   (if deps-entry
                       (cadr deps-entry)
                       '())))
      '()))

;;; audit-deps : Symbol -> DepsAuditReport
;;; Audit dependencies for a skill
(define (audit-deps skill-name)
  (let* ([skill-path (string-append "lattice/" (symbol->string skill-name))]
         [manifest-path (string-append skill-path "/manifest.sexp")]
         [declared-deps (parse-manifest-deps manifest-path)]
         [source-files (find-source-files skill-path)]
         [all-load-deps (apply append (map extract-load-deps source-files))]
         ;; Remove self-references and core references
         [external-deps (filter (lambda (d)
                                        (and (not (eq? d skill-name))
                                             (file-exists? (string-append "lattice/" (symbol->string d) "/manifest.sexp"))))
                                (remove-duplicates all-load-deps))]
         [missing-deps (set-difference external-deps declared-deps)]
         [unused-deps (set-difference declared-deps external-deps)])
        `((skill . ,skill-name)
          (declared-deps . ,declared-deps)
          (actual-deps . ,external-deps)
          (missing-deps . ,missing-deps)
          (unused-deps . ,unused-deps))))

;;; remove-duplicates : (List A) -> (List A)
(define (remove-duplicates lst)
  (let loop ([lst lst] [seen '()] [acc '()])
       (if (null? lst)
           (reverse acc)
           (if (memq (car lst) seen)
               (loop (cdr lst) seen acc)
               (loop (cdr lst) (cons (car lst) seen) (cons (car lst) acc))))))

;;; audit-deps-pretty : Symbol -> void
(define (audit-deps-pretty skill-name)
  (let ([report (audit-deps skill-name)])
       (printf "\n====\n")
       (printf "Dependency Audit: ~a\n" skill-name)
       (printf "====\n\n")
       (printf "Declared deps: ~a\n" (cdr (assq 'declared-deps report)))
       (printf "Actual deps:   ~a\n" (cdr (assq 'actual-deps report)))
       (let ([missing (cdr (assq 'missing-deps report))]
             [unused (cdr (assq 'unused-deps report))])
            (printf "\n")
            (if (null? missing)
                (printf "✓ No undeclared dependencies.\n")
                (begin
                  (printf "✗ Undeclared dependencies (~a):\n" (length missing))
                  (for-each (lambda (d) (printf "  - ~a\n" d)) missing)))
            (printf "\n")
            (if (null? unused)
                (printf "✓ No unused dependencies.\n")
                (begin
                  (printf "? Possibly unused dependencies (~a):\n" (length unused))
                  (for-each (lambda (d) (printf "  - ~a\n" d)) unused))))))

;;; audit-all-deps : -> void
;;; Audit dependencies for all skills
(define (audit-all-deps)
  (printf "\n====\n")
  (printf "Dependency Audit: All Skills\n")
  (printf "====\n\n")
  (let ([skills (kg-skills)])
       (for-each
        (lambda (skill)
                (let* ([report (audit-deps skill)]
                       [missing (cdr (assq 'missing-deps report))])
                      (if (not (null? missing))
                          (printf "~a: missing deps ~a\n" skill missing))))
        skills))
  (printf "\nDone.\n"))

;;; ====
;;; Description Claim Validation
;;; ====

;;; Common data structure/feature terms that should have implementation backing
;;; Format: (phrase-to-match . (evidence-keywords...))
;;; Phrases with spaces are matched exactly; single words match as substrings
(define *feature-terms*
  '(;; Data structures - use specific phrases to avoid false positives
    ("heaps" . (heap priority-queue pq leftist pairing binomial heapsort))
    ("hash table" . (hash-table hashtable hash-map hashmap))
    ("hash tables" . (hash-table hashtable hash-map hashmap))
    ("balanced tree" . (avl-tree red-black rb-tree treemap balanced bst avl))
    ("balanced trees" . (avl-tree red-black rb-tree treemap balanced bst avl))
    ("binary tree" . (binary-tree btree bst avl-tree avl))
    ("trie" . (trie prefix-tree))
    ("graphs" . (graph vertex edge adjacency bfs dfs dijkstra shortest-path))
    ("queues" . (queue fifo enqueue dequeue))
    ("stacks" . (stack lifo push pop))
    ;; "dictionaries" as data structure (avoid matching "dictionary-passing")
    ("dictionaries," . (dict dictionary alist assoc lookup))
    ("sets" . (set member union intersection difference set-))
    ("arrays" . (array vector matrix))
    ;; Algorithms
    ("sorting" . (sort merge-sort quicksort heapsort insertion-sort))
    ("parsers" . (parse parser combinator))
    ("monads" . (monad bind return >>= applicative functor))))

;;; extract-description : Sexp -> String
;;; Extract description string from manifest
(define (extract-description manifest)
  (let* ([body (cddr manifest)]
         [desc-entry (assq 'description body)])
        (if desc-entry
            (let ([desc (cadr desc-entry)])
                 (if (string? desc) desc ""))
            "")))

;;; extract-keywords : Sexp -> (List Symbol)
;;; Extract keywords from manifest
(define (extract-keywords manifest)
  (let* ([body (cddr manifest)]
         [kw-entry (assq 'keywords body)])
        (if kw-entry
            (flatten-symbols (cadr kw-entry))
            '())))

;;; extract-module-names : Sexp -> (List Symbol)
;;; Extract module names from manifest (handles nested subdir format)
(define (extract-module-names manifest)
  (let* ([body (cddr manifest)]
         [mods-entry (assq 'modules body)])
        (if mods-entry
            (flatten-module-names (cdr mods-entry))
            '())))

;;; flatten-module-names : Sexp -> (List Symbol)
;;; Recursively extract module names, handling subdir structures
(define (flatten-module-names mods)
  (cond
   [(null? mods) '()]
   [(symbol? mods) (list mods)]
   [(and (pair? mods) (pair? (car mods)) (eq? (caar mods) 'subdir))
    ;; Subdir format: ((subdir "name") (description ...) (files ...))
    (append (flatten-module-names (cdr mods)))]
   [(and (pair? mods) (symbol? (car mods)))
    ;; Simple module name
    (cons (car mods) (flatten-module-names (cdr mods)))]
   [(and (pair? mods) (pair? (car mods)))
    ;; Nested structure - recurse
    (let ([first (car mods)])
         (if (symbol? (car first))
             (cons (car first) (flatten-module-names (cdr mods)))
             (append (flatten-module-names first)
                     (flatten-module-names (cdr mods)))))]
   [else (flatten-module-names (cdr mods))]))

;;; string-downcase : String -> String
(define (string-downcase s)
  (list->string (map char-downcase (string->list s))))

;;; description-contains? : String String -> Bool
;;; Case-insensitive substring check
(define (description-contains? desc term)
  (string-contains? (string-downcase desc) (string-downcase term)))

;;; find-claimed-features : String -> (List String)
;;; Find feature terms mentioned in description
(define (find-claimed-features description)
  (filter (lambda (term) (description-contains? description term))
          (map car *feature-terms*)))

;;; feature-has-evidence? : String (List Symbol) (List Symbol) -> Bool
;;; Check if a claimed feature has evidence in keywords or modules
(define (feature-has-evidence? feature keywords modules)
  (let* ([evidence-terms (cdr (assoc feature *feature-terms*))]
         [all-names (append keywords modules)]
         [all-strings (map symbol->string all-names)])
        ;; Check if any evidence term appears in keywords/modules
        (any (lambda (evidence)
                    (let ([ev-str (symbol->string evidence)])
                         (any (lambda (name)
                                     (or (string-contains? (string-downcase name) ev-str)
                                         (string=? (string-downcase name) ev-str)))
                              all-strings)))
             evidence-terms)))

;;; any : (A -> Bool) (List A) -> Bool
(define (any pred lst)
  (cond
   [(null? lst) #f]
   [(pred (car lst)) #t]
   [else (any pred (cdr lst))]))

;;; audit-description : Symbol -> DescriptionAuditReport
;;; Check if skill description claims match implementation
(define (audit-description skill-name)
  (let* ([skill-path (string-append "lattice/" (symbol->string skill-name))]
         [manifest-path (string-append skill-path "/manifest.sexp")]
         [manifest (if (file-exists? manifest-path)
                       (read-manifest manifest-path)
                       #f)])
        (if manifest
            (let* ([description (extract-description manifest)]
                   [keywords (extract-keywords manifest)]
                   [modules (extract-module-names manifest)]
                   [claimed (find-claimed-features description)]
                   [unfounded (filter (lambda (f)
                                             (not (feature-has-evidence? f keywords modules)))
                                      claimed)]
                   [supported (filter (lambda (f)
                                             (feature-has-evidence? f keywords modules))
                                      claimed)])
                  `((skill . ,skill-name)
                    (claimed-features . ,claimed)
                    (supported-features . ,supported)
                    (unfounded-claims . ,unfounded)
                    (keywords . ,keywords)
                    (modules . ,modules)))
            `((skill . ,skill-name)
              (error . "manifest not found")))))

;;; audit-description-pretty : Symbol -> void
;;; Pretty-print description audit
(define (audit-description-pretty skill-name)
  (let ([report (audit-description skill-name)])
       (printf "\n====\n")
       (printf "Description Audit: ~a\n" skill-name)
       (printf "====\n\n")
       (if (assq 'error report)
           (printf "Error: ~a\n" (cdr (assq 'error report)))
           (let ([claimed (cdr (assq 'claimed-features report))]
                 [supported (cdr (assq 'supported-features report))]
                 [unfounded (cdr (assq 'unfounded-claims report))])
                (printf "Features mentioned in description: ~a\n" (length claimed))
                (for-each (lambda (f) (printf "  - ~a\n" f)) claimed)
                (printf "\n")
                (if (null? unfounded)
                    (printf "✓ All claimed features have implementation evidence.\n")
                    (begin
                      (printf "✗ UNFOUNDED CLAIMS (~a):\n" (length unfounded))
                      (for-each (lambda (f)
                                        (printf "  - \"~a\" — no matching keywords or modules\n" f))
                                unfounded)
                      (printf "\n  These features are mentioned in the description but have no\n")
                      (printf "  corresponding keywords or module names. Either implement them\n")
                      (printf "  or remove from description.\n")))))))

;;; audit-all-descriptions : -> void
;;; Audit descriptions for all skills, showing only problems
(define (audit-all-descriptions)
  (printf "\n====\n")
  (printf "Description Audit: All Skills\n")
  (printf "====\n\n")
  (let ([skills (kg-skills)]
        [problems 0])
       (for-each
        (lambda (skill)
                (let* ([report (audit-description skill)]
                       [unfounded (if (assq 'unfounded-claims report)
                                      (cdr (assq 'unfounded-claims report))
                                      '())])
                      (when (not (null? unfounded))
                            (set! problems (+ problems 1))
                            (printf "~a: unfounded claims ~a\n" skill unfounded))))
        skills)
       (printf "\n")
       (if (= problems 0)
           (printf "✓ All skills have honest descriptions.\n")
           (printf "✗ ~a skill(s) have unfounded claims.\n" problems))))

;;; ====
;;; REPL Interface
;;; ====

(meta-printf "audit.ss loaded.\n")
(meta-printf "  (audit-skill 'skill)           - Audit single skill\n")
(meta-printf "  (audit-skill-pretty 'skill)    - Pretty-print audit\n")
(meta-printf "  (audit-file \"path\")            - Audit single file\n")
(meta-printf "  (suggest-exports 'skill)       - Generate exports clause\n")
(meta-printf "  (suggest-missing 'skill)       - List missing exports\n")
(meta-printf "  (audit-deps 'skill)            - Audit dependencies\n")
(meta-printf "  (audit-deps-pretty 'skill)     - Pretty-print dep audit\n")
(meta-printf "  (audit-all-deps)               - Audit all skills' deps\n")
(meta-printf "  (audit-description 'skill)     - Check description claims\n")
(meta-printf "  (audit-description-pretty 'skill) - Pretty-print claim audit\n")
(meta-printf "  (audit-all-descriptions)       - Check all skill descriptions\n")
