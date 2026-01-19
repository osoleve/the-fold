;;; boundary/lens/jump.ss — Navigation Primitives
;;;
;;; Low-level navigation utilities for jumping to definitions,
;;; locating references, and opening files at specific locations.
;;;
;;; This is Shell code: uses symbol index, formats output.
;;;
;;; Usage:
;;;   (jump-to-def 'sym)         - Jump to symbol definition
;;;   (jump-to-file path line)   - Format file:line location
;;;   (jump-location 'sym)       - Get structured location data
;;;
;;; Dependencies:
;;;   boundary/tools/index.ss (symbol index)

;;; ====
;;; Location Types
;;; ====

;;; location : String × Nat × Nat -> Location
;;; Create a location record: (file line column)
(define (make-location file line column)
  `((file . ,file)
    (line . ,line)
    (column . ,column)))

;;; location-file : Location -> String
(define (location-file loc)
  (cdr (assq 'file loc)))

;;; location-line : Location -> Nat
(define (location-line loc)
  (cdr (assq 'line loc)))

;;; location-column : Location -> Nat
(define (location-column loc)
  (cdr (assq 'column loc)))

;;; location->string : Location -> String
;;; Format location as "file:line:column" or "file:line".
(define (location->string loc)
  (let ([file (location-file loc)]
        [line (location-line loc)]
        [col (location-column loc)])
       (if (= col 0)
           (format "~a:~a" file line)
           (format "~a:~a:~a" file line col))))

;;; ====
;;; Symbol Location Lookup
;;; ====

;;; jump-location : Symbol -> Location | #f
;;; Get the location of a symbol's definition.
(define (jump-location sym)
  (let ([entry (guard (e [else #f])
                      (and (top-level-bound? 'index-lookup)
                           (procedure? index-lookup)
                           (index-lookup sym)))])
       (if entry
           (make-location
            (cdr (assq 'file entry))
            (cdr (assq 'line entry))
            0)  ; Column not tracked in index
           #f)))

;;; jump-locations : Symbol -> (List Location)
;;; Get all definition locations for a symbol (handles redefinitions).
(define (jump-locations sym)
  (let ([loc (jump-location sym)])
       (if loc (list loc) '())))

;;; ====
;;; Jump Commands
;;; ====

;;; jump-to-def : Symbol -> void
;;; Display the definition location for a symbol.
(define (jump-to-def sym)
  (let ([loc (jump-location sym)])
       (display "\n")
       (if loc
           (begin
            (printf "  ~a defined at:\n" sym)
            (display "  ────────────────────────────────\n")
            (printf "    ~a\n" (location->string loc))
            (display "\n")
            ;; Show a preview of the definition
            (show-definition-preview loc))
           (printf "  Symbol '~a' not found in index.\n\n" sym))))

;;; show-definition-preview : Location -> void
;;; Display a few lines around the definition.
(define (show-definition-preview loc)
  (guard (e [else (void)])
         (let* ([file (location-file loc)]
                [line (location-line loc)]
                [lines (read-file-lines file)]
                [start (max 0 (- line 1))]
                [end (min (length lines) (+ line 4))])
               (display "  Preview:\n")
               (display "  ────────────────────────────────\n")
               (do ([i start (+ i 1)])
                   ((>= i end))
                   (let ([line-content (list-ref lines i)]
                         [line-num (+ i 1)]
                         [marker (if (= (+ i 1) line) "→" " ")])
                        (printf "  ~a ~4d │ ~a\n" marker line-num line-content)))
               (display "\n"))))

;;; read-file-lines : String -> (List String)
;;; Read all lines from a file.
(define (read-file-lines path)
  (guard (e [else '()])
         (call-with-input-file path
                               (lambda (port)
                                       (let loop ([lines '()])
                                            (let ([line (get-line port)])
                                                 (if (eof-object? line)
                                                     (reverse lines)
                                                     (loop (cons line lines)))))))))

;;; ====
;;; Reference Lookup
;;; ====

;;; jump-refs : Symbol -> (List Location)
;;; Get all locations where a symbol is referenced.
;;; Uses callers from call graph as a proxy.
(define (jump-refs sym)
  (let ([callers (call-graph-callers sym)])
       (filter-map-jump
        (lambda (caller)
                (jump-location caller))
        callers)))

;;; filter-map-jump : (α -> β | #f) × (List α) -> (List β)
(define (filter-map-jump f lst)
  (let loop ([lst lst] [acc '()])
       (if (null? lst)
           (reverse acc)
           (let ([result (f (car lst))])
                (if result
                    (loop (cdr lst) (cons result acc))
                    (loop (cdr lst) acc))))))

;;; jump-to-refs : Symbol -> void
;;; Display all reference locations for a symbol.
(define (jump-to-refs sym)
  (let* ([callers (call-graph-callers sym)]
         [refs (jump-refs sym)])
        (display "\n")
        (printf "  References to '~a':\n" sym)
        (display "  ────────────────────────────────\n")
        (if (null? refs)
            (display "    (no references found)\n")
            (for-each
             (lambda (caller loc)
                     (printf "    ~a\n" (location->string loc))
                     (printf "      in ~a\n" caller))
             callers refs))
        (display "\n")))

;;; ====
;;; Multi-Location Navigation
;;; ====

;;; jump-list : Symbol -> void
;;; Show a numbered list of all locations related to a symbol.
(define (jump-list sym)
  (let* ([def-loc (jump-location sym)]
         [ref-locs (jump-refs sym)]
         [test-locs (and (procedure? find-tests-for)
                         (map (lambda (t) (make-location (car t) 1 0))
                              (find-tests-for sym)))]
         [all-locs (append
                    (if def-loc (list (cons 'definition def-loc)) '())
                    (map (lambda (l) (cons 'reference l)) ref-locs)
                    (map (lambda (l) (cons 'test l)) (or test-locs '())))])
        (display "\n")
        (printf "  Locations for '~a':\n" sym)
        (display "  ────────────────────────────────\n")
        (if (null? all-locs)
            (display "    (no locations found)\n")
            (let loop ([locs all-locs] [n 1])
                 (unless (null? locs)
                         (let* ([entry (car locs)]
                                [type (car entry)]
                                [loc (cdr entry)])
                               (printf "    [~a] ~a (~a)\n"
                                       n
                                       (location->string loc)
                                       type)
                               (loop (cdr locs) (+ n 1))))))
        (display "\n")))

;;; ====
;;; Quick Navigation Helpers
;;; ====

;;; jump-file-line : String × Nat -> String
;;; Format as file:line for editor integration.
(define (jump-file-line file line)
  (format "~a:~a" file line))

;;; jump-emacs-format : Location -> String
;;; Format for Emacs: +line file
(define (jump-emacs-format loc)
  (format "+~a ~a" (location-line loc) (location-file loc)))

;;; jump-vim-format : Location -> String
;;; Format for Vim: file +line
(define (jump-vim-format loc)
  (format "~a +~a" (location-file loc) (location-line loc)))

;;; jump-vscode-format : Location -> String
;;; Format for VSCode: file:line:column
(define (jump-vscode-format loc)
  (location->string loc))

(display "Jump navigation module loaded.\n")
