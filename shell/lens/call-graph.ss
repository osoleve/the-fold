;;; shell/lens/call-graph.ss — Call Graph Builder
;;;
;;; Builds forward and reverse call graphs from the codebase.
;;; Used by lens navigation for finding callers/callees.
;;;
;;; This is Shell code: reads files, builds mutable indices.
;;;
;;; Usage:
;;;   (call-graph-refresh!)       - Rebuild call graph from source
;;;   (call-graph-callers 'sym)   - Get symbols that call sym
;;;   (call-graph-callees 'sym)   - Get symbols that sym calls
;;;   (call-graph-stats)          - Show call graph statistics
;;;
;;; Dependencies:
;;;   shell/tools/index.ss (symbol index)
;;;   shell/xref.ss (cross-reference utilities)

;;; ====
;;; Call Graph Data Structures
;;; ====

;;; Forward graph: symbol -> (list of symbols it calls)
(define *forward-calls* (make-eq-hashtable))

;;; Reverse graph: symbol -> (list of symbols that call it)
(define *reverse-calls* (make-eq-hashtable))

;;; Call site info: (caller callee file line)
(define *call-sites* '())

;;; Index status
(define *call-graph-built?* #f)

;;; ====
;;; S-Expression Analysis
;;; ====

;;; extract-calls-from-body : S-expr Symbol -> (List Symbol)
;;; Extract all symbol references from an expression body.
;;; Excludes the defining symbol itself and local bindings.
(define (extract-calls-from-body expr defining-sym)
  (let ([locals (make-eq-hashtable)]
        [calls '()])
       ;; Add defining-sym to locals to avoid self-reference noise
       (hashtable-set! locals defining-sym #t)
       (letrec
        ([walk
          (lambda (expr)
                  (cond
                   [(symbol? expr)
                    ;; Only include if not a local binding
                    (unless (hashtable-ref locals expr #f)
                            (set! calls (cons expr calls)))]
                   
                   [(not (pair? expr)) (void)]
                   
                   ;; Handle binding forms - add to locals before walking body
                   [(and (pair? expr)
                         (symbol? (car expr))
                         (memq (car expr) '(lambda λ)))
                    ;; (lambda (args...) body...)
                    (when (pair? (cdr expr))
                          (let ([params (cadr expr)])
                               ;; Register parameters as locals
                               (cond
                                [(symbol? params)
                                 (hashtable-set! locals params #t)]
                                [(pair? params)
                                 (for-each
                                  (lambda (p)
                                          (when (symbol? p)
                                                (hashtable-set! locals p #t)))
                                  params)]))
                          ;; Walk the body
                          (for-each walk (cddr expr)))]
                   
                   [(and (pair? expr)
                         (symbol? (car expr))
                         (memq (car expr) '(let let* letrec letrec*)))
                    ;; Handle let forms
                    (when (and (pair? (cdr expr))
                               (pair? (cadr expr)))
                          (let ([bindings (if (symbol? (cadr expr))
                                              ;; Named let
                                              (begin
                                               (hashtable-set! locals (cadr expr) #t)
                                               (caddr expr))
                                              (cadr expr))]
                                [body (if (symbol? (cadr expr))
                                          (cdddr expr)
                                          (cddr expr))])
                               ;; Register binding names
                               (when (list? bindings)
                                     (for-each
                                      (lambda (b)
                                              (when (and (pair? b) (symbol? (car b)))
                                                    (hashtable-set! locals (car b) #t)
                                                    ;; Walk the init expression
                                                    (when (pair? (cdr b))
                                                          (walk (cadr b)))))
                                      bindings))
                               ;; Walk body
                               (for-each walk body)))]
                   
                   [(and (pair? expr)
                         (symbol? (car expr))
                         (eq? (car expr) 'define))
                    ;; Handle nested define - register and walk
                    (when (pair? (cdr expr))
                          (let ([name-part (cadr expr)])
                               (cond
                                [(symbol? name-part)
                                 (hashtable-set! locals name-part #t)]
                                [(pair? name-part)
                                 (hashtable-set! locals (car name-part) #t)])
                               (when (pair? (cddr expr))
                                     (walk (caddr expr)))))]
                   
                   [(pair? expr)
                    ;; General case - walk all subexpressions
                    (walk (car expr))
                    (walk (cdr expr))]))])
        (walk expr)
        (remove-duplicates-eq calls))))

;;; remove-duplicates-eq : (List Symbol) -> (List Symbol)
;;; Uses eq-hashtable for O(n) symbol deduplication.
;;; Faster than prelude's remove-duplicates for symbol-only lists.
(define (remove-duplicates-eq lst)
  (let ([seen (make-eq-hashtable)])
       (filter
        (lambda (x)
                (if (hashtable-ref seen x #f)
                    #f
                    (begin
                     (hashtable-set! seen x #t)
                     #t)))
        lst)))

;;; ====
;;; Definition Extraction
;;; ====

;;; extract-definition-info : S-expr -> (Symbol S-expr) | #f
;;; Extract (name body) from a definition form.
(define (extract-definition-info expr)
  (cond
   [(not (pair? expr)) #f]
   [(not (memq (car expr) '(define define-syntax))) #f]
   [(not (pair? (cdr expr))) #f]
   [else
    (let ([name-part (cadr expr)]
          [body (cddr expr)])
         (cond
          ;; (define name value)
          [(and (symbol? name-part) (pair? body))
           (cons name-part (car body))]
          ;; (define (name args...) body...)
          [(and (pair? name-part) (symbol? (car name-part)))
           (cons (car name-part)
                 `(lambda ,(cdr name-part) ,@body))]
          [else #f]))]))

;;; ====
;;; File Processing
;;; ====

;;; process-file-for-calls : String -> void
;;; Process a single file to extract call relationships.
(define (process-file-for-calls path)
  (guard (e [else (void)])  ; Skip files with parse errors
         (call-with-input-file path
                               (lambda (port)
                                       (let loop ()
                                            (let ([expr (guard (e [else #f]) (read port))])
                                                 (cond
                                                  [(eof-object? expr) (void)]
                                                  [(not expr) (void)]  ; Parse error
                                                  [else
                                                   (let ([info (extract-definition-info expr)])
                                                        (when info
                                                              (let* ([definer (car info)]
                                                                     [body (cdr info)]
                                                                     [callees (extract-calls-from-body body definer)])
                                                                    ;; Update forward graph
                                                                    (hashtable-set! *forward-calls* definer callees)
                                                                    ;; Update reverse graph
                                                                    (for-each
                                                                     (lambda (callee)
                                                                             (let ([existing (hashtable-ref *reverse-calls* callee '())])
                                                                                  (hashtable-set! *reverse-calls* callee
                                                                                                  (cons definer existing))))
                                                                     callees))))
                                                   (loop)])))))))

;;; find-scheme-files : String -> (List String)
;;; Recursively find all .ss files in directory.
(define (find-scheme-files dir)
  (define (skip-dir? name)
    (or (string=? name ".")
        (string=? name "..")
        (string=? name ".git")
        (string=? name ".fold-repl")
        (string=? name ".fold-sessions")
        (string=? name "node_modules")
        (string=? name ".store")))
  
  (guard (e [else '()])
         (let loop ([pending (list dir)] [results '()])
              (if (null? pending)
                  results
                  (let ([current (car pending)]
                        [rest (cdr pending)])
                       (cond
                        [(not (file-exists? current))
                         (loop rest results)]
                        [(file-directory? current)
                         (let ([entries (guard (e [else '()])
                                               (directory-list current))])
                              (let ([full-paths
                                     (filter-map
                                      (lambda (name)
                                              (and (not (skip-dir? name))
                                                   (string-append current "/" name)))
                                      entries)])
                                   (loop (append full-paths rest) results)))]
                        [(string-suffix? ".ss" current)
                         (loop rest (cons current results))]
                        [else
                         (loop rest results)]))))))

;;; filter-map : (α -> β | #f) × (List α) -> (List β)
(define (filter-map f lst)
  (let loop ([lst lst] [acc '()])
       (if (null? lst)
           (reverse acc)
           (let ([result (f (car lst))])
                (if result
                    (loop (cdr lst) (cons result acc))
                    (loop (cdr lst) acc))))))

;;; string-suffix? is provided by shell/string-utils.ss with signature (suffix str)

;;; ====
;;; Public API
;;; ====

;;; call-graph-refresh! : -> void
;;; Rebuild the entire call graph from source files.
(define (call-graph-refresh!)
  ;; Clear existing data
  (hashtable-clear! *forward-calls*)
  (hashtable-clear! *reverse-calls*)
  (set! *call-sites* '())
  
  ;; Process each directory
  (display "Building call graph...\n")
  (display "  Scanning core/...\n")
  (for-each process-file-for-calls (find-scheme-files "core"))
  (display "  Scanning shell/...\n")
  (for-each process-file-for-calls (find-scheme-files "shell"))
  (display "  Scanning user/...\n")
  (for-each process-file-for-calls (find-scheme-files "user"))
  
  (set! *call-graph-built?* #t)
  (call-graph-stats))

;;; call-graph-callers : Symbol -> (List Symbol)
;;; Get all symbols that call the given symbol.
(define (call-graph-callers sym)
  (unless *call-graph-built?*
          (call-graph-refresh!))
  (hashtable-ref *reverse-calls* sym '()))

;;; call-graph-callees : Symbol -> (List Symbol)
;;; Get all symbols that the given symbol calls.
(define (call-graph-callees sym)
  (unless *call-graph-built?*
          (call-graph-refresh!))
  (hashtable-ref *forward-calls* sym '()))

;;; call-graph-path : Symbol × Symbol × Nat -> (List (List Symbol)) | #f
;;; Find call paths from src to dst, up to max-depth.
;;; Returns list of paths (each path is a list of symbols).
(define (call-graph-path src dst max-depth)
  (unless *call-graph-built?*
          (call-graph-refresh!))
  (letrec
   ([search
     (lambda (current path depth)
             (cond
              [(> depth max-depth) '()]
              [(eq? current dst) (list (reverse (cons dst path)))]
              [(member current path) '()]  ; Cycle detection
              [else
               (let ([callees (hashtable-ref *forward-calls* current '())])
                    (apply append
                           (map (lambda (next)
                                        (search next (cons current path) (+ depth 1)))
                                callees)))]))])
   (let ([paths (search src '() 0)])
        (if (null? paths) #f paths))))

;;; call-graph-stats : -> void
;;; Display call graph statistics.
(define (call-graph-stats)
  (display "\n")
  (display "  Call Graph Statistics\n")
  (display "  ────────────────────────────────\n")
  (printf "  Functions indexed:    ~a\n" (hashtable-size *forward-calls*))
  (printf "  Unique call targets:  ~a\n" (hashtable-size *reverse-calls*))
  (let ([total-edges 0])
       (vector-for-each
        (lambda (key)
                (set! total-edges
                      (+ total-edges
                         (length (hashtable-ref *forward-calls* key '())))))
        (hashtable-keys *forward-calls*))
       (printf "  Total call edges:     ~a\n" total-edges))
  (display "\n"))

;;; call-graph-built? : -> Boolean
;;; Check if call graph has been built.
(define (call-graph-built?)
  *call-graph-built?*)

(display "Call graph module loaded.\n")
