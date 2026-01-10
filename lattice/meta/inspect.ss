;;; lattice/meta/inspect.ss — Skill Introspection
;;;
;;; Detailed inspection of skills, modules, and exports.
;;; Provides structured information for agent consumption.
;;;
;;; This is Lattice code: pure (mostly), uses Core primitives.
;;;
;;; Usage:
;;;   (lattice-describe 'skill)         ; Full skill description
;;;   (lattice-exports 'skill)          ; List all exports
;;;   (lattice-modules 'skill)          ; List all modules
;;;   (lattice-signature 'export)       ; Type signature (if available)
;;;
;;; Dependencies:
;;;   lattice/meta/kg.ss
;;;   lattice/meta/dag.ss

(load "lattice/meta/dag.ss")

;;; ============================================================
;;; Skill Description
;;; ============================================================

;;; lattice-describe : Symbol -> void
;;; Pretty-print full skill description
(define (lattice-describe skill-name)
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
                         (let ([mod-name (car mod)])
                              (printf "  - ~a\n" mod-name)))
                 modules)))))

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

;;; ============================================================
;;; Export Listing
;;; ============================================================

;;; lattice-skill-exports : Symbol -> (List Symbol)
;;; Get all exports from a skill
(define (lattice-skill-exports skill-name)
  (let* ([data (kg-skill-data skill-name)]
         [exports-raw (if data
                          (cdr (or (assq 'exports data) '(exports . ())))
                          '())])
        ;; Flatten export groups
        (apply append
               (map (lambda (group)
                            (if (and (pair? group) (list? group))
                                (cdr group)  ; Skip module name
                                '()))
                    (if (list? exports-raw) exports-raw '())))))

;;; lattice-all-exports : -> (List (skill . (exports ...)))
;;; Get all exports grouped by skill
(define (lattice-all-exports)
  (map (lambda (skill-name)
               (cons skill-name (lattice-skill-exports skill-name)))
       (kg-skills)))

;;; lattice-exports-pretty : Symbol -> void
;;; Pretty-print exports for a skill
(define (lattice-exports-pretty skill-name)
  (let ([data (kg-skill-data skill-name)])
       (if (not data)
           (printf "Skill not found: ~a\n" skill-name)
           (let ([exports-raw (cdr (or (assq 'exports data) '(exports . ())))])
                (printf "Exports for ~a\n" skill-name)
                (printf "~a\n\n" (make-string 40 #\-))
                (for-each
                 (lambda (group)
                         (when (and (pair? group) (list? group))
                               (printf "~a:\n" (car group))
                               (for-each
                                (lambda (exp)
                                        (printf "  ~a\n" exp))
                                (cdr group))
                               (printf "\n")))
                 (if (list? exports-raw) exports-raw '()))))))

;;; ============================================================
;;; Module Listing
;;; ============================================================

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

;;; ============================================================
;;; Source Location
;;; ============================================================

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

;;; ============================================================
;;; Agent-Friendly Info
;;; ============================================================

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
  (printf "~20a ~8a ~8a ~6a ~a\n" "Skill" "Tier" "Purity" "Mods" "Description")
  (printf "~20a ~8a ~8a ~6a ~a\n" "--------------------" "--------" "--------" "------" "------------------------")
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
   (kg-skills)))

;;; truncate-string : String Int -> String
(define (truncate-string str max-len)
  (let ([s (string-trim str)])
       (if (<= (string-length s) max-len)
           s
           (string-append (substring s 0 (- max-len 3)) "..."))))

;;; ============================================================
;;; Convenience Functions (for REPL)
;;; ============================================================

;;; li : Symbol -> void
;;; Quick skill inspection
(define (li skill-name)
  (lattice-describe skill-name))

;;; le : Symbol -> void
;;; Quick exports list
(define (le skill-name)
  (lattice-exports-pretty skill-name))

;;; lm : Symbol -> void
;;; Quick modules list
(define (lm skill-name)
  (lattice-modules-detail skill-name))

;;; ============================================================
;;; REPL Interface
;;; ============================================================

(printf "inspect.ss loaded.\n")
(printf "  (lattice-describe 'skill)     - Full description\n")
(printf "  (lattice-skill-exports 'skill) - Export list\n")
(printf "  (lattice-modules-detail 'skill) - Module details\n")
(printf "  (lattice-source 'export)      - Source location\n")
(printf "  (lattice-info 'skill)         - Structured info\n")
(printf "  (lattice-summary)             - All skills summary\n")
(printf "  (li 'skill), (le 'skill)      - Quick inspection\n")
