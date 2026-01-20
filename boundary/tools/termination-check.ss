(load "core/base/prelude.ss")

(doc 'module 'termination-check)
(doc 'description "REPL integration for termination checker with call graph analysis and batch reporting")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(load "core/types/types.ss")
(load "core/types/dep-types.ss")
(load "core/types/termination.ss")

(doc 'section 'termination-analysis)

(define (terminates? expr params)
  (doc 'type "(-> Expr (List Symbol) Bool)")
  (doc 'description "Check if expression terminates; returns #t if proven, #f otherwise")
  (let ([result (check-termination expr params)])
       (if (eq? (car result) 'ok)
           (begin
            (display "\n  ✓ Termination proven.\n\n")
            #t)
           (begin
            (display "\n  ✗ Cannot prove termination.\n")
            (display "    Reason: ")
            (display (cadr result))
            (display "\n")
            (when (assq 'hint (cdr result))
                  (display "    Hint: ")
                  (display (cdr (assq 'hint (cdr result))))
                  (display "\n"))
            (display "\n")
            #f))))

;;; show-call-graph : Expr × (List Symbol) → Void
;;; Display the recursive call structure of an expression.
(define (show-call-graph expr params)
  (let* ([cg (extract-call-graph expr params)]
         [name (call-graph-name cg)]
         [cg-params (call-graph-params cg)]
         [edges (call-graph-edges cg)])
        (display "\n")
        (display "  Call Graph Analysis\n")
        (display "  ────────────────────────────────────────────\n")
        (printf "  Function: ~a\n" name)
        (printf "  Parameters: ~a\n" cg-params)
        (display "\n")
        (if (null? edges)
            (display "  No recursive calls detected.\n")
            (begin
             (printf "  Recursive calls (~a):\n" (length edges))
             (for-each
              (lambda (edge)
                      (let ([src (call-edge-source edge)]
                            [tgt (call-edge-target edge)]
                            [changes (call-edge-size-changes edge)])
                           (printf "    ~a → ~a\n" src tgt)
                           (for-each
                            (lambda (change)
                                    (let ([idx (car change)]
                                          [rel (cdr change)])
                                         (printf "      param[~a]: ~a\n"
                                                 idx (relation->string rel))))
                            changes)))
              edges)))
        (display "\n")))

;;; relation->string : (Idx . Relation) → String
(define (relation->string rel)
  (let ([target-idx (if (pair? (cdr rel)) (cadr rel) "?")]
        [relation (if (pair? (cdr rel)) (cddr rel) (cdr rel))])
       (case relation
             [(<) (format "strictly decreasing (maps to param[~a])" target-idx)]
             [(<=) (format "non-increasing (maps to param[~a])" target-idx)]
             [(?) "unknown relation"]
             [else (format "~a" relation)])))

;;; check-function : Expr → Void
;;; Analyze a function definition for termination.
;;; Input: (define (name params...) body) or (define name (fn (params...) body))
(define (check-function def)
  (cond
   ;; (define (name params...) body)
   [(and (pair? def) (eq? (car def) 'define) (pair? (cadr def)))
    (let* ([name (caadr def)]
           [params (cdadr def)]
           [body (caddr def)])
          (display "\n")
          (printf "  Analyzing function: ~a\n" name)
          (printf "  Parameters: ~a\n" params)
          (display "  ────────────────────────────────────────────\n")
          (let ([result (check-termination body params)])
               (if (eq? (car result) 'ok)
                   (display "  ✓ Function terminates on all inputs.\n")
                   (begin
                    (display "  ✗ Cannot prove termination.\n")
                    (when (assq 'hint (cdr result))
                          (printf "    Hint: ~a\n" (cdr (assq 'hint (cdr result))))))))
          (display "\n"))]
   
   ;; (define name (fn (params...) body))
   [(and (pair? def) (eq? (car def) 'define)
         (symbol? (cadr def))
         (pair? (caddr def))
         (memq (car (caddr def)) '(fn λ lambda)))
    (let* ([name (cadr def)]
           [fn-expr (caddr def)]
           [params (cadr fn-expr)]
           [body (caddr fn-expr)])
          (display "\n")
          (printf "  Analyzing function: ~a\n" name)
          (printf "  Parameters: ~a\n" params)
          (display "  ────────────────────────────────────────────\n")
          (let ([result (check-termination body params)])
               (if (eq? (car result) 'ok)
                   (display "  ✓ Function terminates on all inputs.\n")
                   (begin
                    (display "  ✗ Cannot prove termination.\n")
                    (when (assq 'hint (cdr result))
                          (printf "    Hint: ~a\n" (cdr (assq 'hint (cdr result))))))))
          (display "\n"))]
   
   ;; (fix name body)
   [(and (pair? def) (eq? (car def) 'fix))
    (let* ([name (cadr def)]
           [body (caddr def)])
          (display "\n")
          (printf "  Analyzing fixpoint: ~a\n" name)
          (display "  ────────────────────────────────────────────\n")
          (let ([result (check-termination body '())])
               (if (eq? (car result) 'ok)
                   (display "  ✓ Fixpoint terminates.\n")
                   (display "  ✗ Cannot prove termination.\n")))
          (display "\n"))]
   
   [else
    (display "\n  Error: Not a recognized function definition.\n")
    (display "  Expected: (define (name params...) body)\n")
    (display "        or: (define name (fn (params...) body))\n")
    (display "        or: (fix name body)\n\n")]))

;;; ====
;;; Structural Recursion Analysis
;;; ====

;;; analyze-recursion : Expr × Symbol × Symbol → Void
;;; Show how a recursive function uses its recursive argument.
(define (analyze-recursion expr func-name rec-param)
  (let ([violations (find-recursion-violations func-name rec-param expr '())])
       (display "\n")
       (display "  Structural Recursion Analysis\n")
       (display "  ────────────────────────────────────────────\n")
       (printf "  Function: ~a, Recursive parameter: ~a\n" func-name rec-param)
       (display "\n")
       (if (null? violations)
           (display "  ✓ All recursive calls use structural subterms.\n")
           (begin
            (printf "  ✗ Found ~a non-structural recursive call(s):\n" (length violations))
            (for-each
             (lambda (v)
                     (printf "    - ~s\n" v))
             violations)))
       (display "\n")))

;;; ====
;;; Size-Change Graph Visualization
;;; ====

;;; show-scg : Expr × (List Symbol) → Void
;;; Display size-change graphs for recursive calls.
(define (show-scg expr params)
  (let* ([cg (extract-call-graph expr params)]
         [scgs (call-graph->scgs cg)])
        (display "\n")
        (display "  Size-Change Graphs\n")
        (display "  ────────────────────────────────────────────\n")
        (if (null? scgs)
            (display "  No size-change graphs (no recursive calls).\n")
            (let loop ([gs scgs] [idx 1])
                 (unless (null? gs)
                         (let* ([g (car gs)]
                                [src (scg-source-params g)]
                                [tgt (scg-target-params g)]
                                [edges (scg-edges g)])
                               (printf "\n  SCG #~a:\n" idx)
                               (printf "    Source params: ~a\n" src)
                               (printf "    Target params: ~a\n" tgt)
                               (display "    Matrix:\n")
                               (let row-loop ([rows edges] [i 0])
                                    (unless (null? rows)
                                            (printf "      [~a]: ~a\n"
                                                    (if (< i (length src))
                                                        (list-ref src i)
                                                        i)
                                                    (map symbol->string (car rows)))
                                            (row-loop (cdr rows) (+ i 1)))))
                         (loop (cdr gs) (+ idx 1)))))
        (display "\n")
        ;; Check termination
        (if (all-scgs-terminate? scgs)
            (display "  ✓ Size-change termination criterion satisfied.\n")
            (display "  ✗ Size-change termination criterion NOT satisfied.\n"))
        (display "\n")))

;;; ====
;;; Batch Analysis
;;; ====

;;; termination-report : String → Void
;;; Analyze all function definitions in a file.
(define (termination-report path)
  (guard (e [else (printf "\n  Error reading file: ~a\n\n" path)])
         (let* ([content (call-with-input-file path get-string-all)]
                [port (open-input-string content)]
                [definitions (read-definitions port)])
               (display "\n")
               (printf "  Termination Report: ~a\n" path)
               (display "  ════════════════════════════════════════════\n")
               (printf "  Found ~a definition(s)\n\n" (length definitions))
               (let loop ([defs definitions] [pass 0] [fail 0])
                    (if (null? defs)
                        (begin
                         (display "  ────────────────────────────────────────────\n")
                         (printf "  Summary: ~a pass, ~a fail\n" pass fail)
                         (display "\n"))
                        (let* ([def (car defs)]
                               [result (analyze-definition def)])
                              (loop (cdr defs)
                                    (if result (+ pass 1) pass)
                                    (if result fail (+ fail 1)))))))))

;;; read-definitions : Port → (List Expr)
;;; Read all s-expressions from port, filtering for definitions.
(define (read-definitions port)
  (let loop ([defs '()])
       (let ([expr (read port)])
            (if (eof-object? expr)
                (reverse defs)
                (if (and (pair? expr) (eq? (car expr) 'define))
                    (loop (cons expr defs))
                    (loop defs))))))

;;; analyze-definition : Expr → Boolean
;;; Analyze a single definition, return #t if it passes.
(define (analyze-definition def)
  (cond
   ;; (define (name params...) body)
   [(and (pair? def) (eq? (car def) 'define) (pair? (cadr def)))
    (let* ([name (caadr def)]
           [params (cdadr def)]
           [body (caddr def)]
           [result (check-termination body params)])
          (if (eq? (car result) 'ok)
              (begin
               (printf "  ✓ ~a\n" name)
               #t)
              (begin
               (printf "  ✗ ~a (cannot prove termination)\n" name)
               #f)))]
   ;; (define name (fn (params...) body)) or (define name value)
   [(and (pair? def) (eq? (car def) 'define) (symbol? (cadr def)))
    (let ([name (cadr def)]
          [value (caddr def)])
         (if (and (pair? value) (memq (car value) '(fn λ lambda)))
             ;; Lambda-style: (define name (fn (params...) body))
             (let* ([params (cadr value)]
                    [body (caddr value)]
                    [result (check-termination body params)])
                   (if (eq? (car result) 'ok)
                       (begin (printf "  ✓ ~a\n" name) #t)
                       (begin (printf "  ✗ ~a (cannot prove termination)\n" name) #f)))
             ;; Not a function
             (begin
              (printf "  - ~a (not a function)\n" name)
              #t)))]
   [else
    (display "  - (unrecognized form)\n")
    #t]))

;;; ====
;;; Help
;;; ====

(define (termination-help)
  (display "\n")
  (display "  Termination Checker Commands\n")
  (display "  ────────────────────────────────────────────\n")
  (display "  (terminates? expr params)    - Check termination\n")
  (display "  (check-function def)         - Analyze function definition\n")
  (display "  (show-call-graph expr params)- Show recursive call structure\n")
  (display "  (show-scg expr params)       - Show size-change graphs\n")
  (display "  (analyze-recursion expr f p) - Check structural recursion\n")
  (display "  (termination-report path)    - Analyze all functions in file\n")
  (display "\n")
  (display "  Examples:\n")
  (display "    (check-function\n")
  (display "      '(define (factorial n)\n")
  (display "         (if (= n 0) 1 (* n (factorial (- n 1))))))\n")
  (display "\n")
  (display "    (show-call-graph '(if (= n 0) 1 (* n (f (- n 1)))) '(n))\n")
  (display "\n"))

(display "Termination checker tooling loaded. Use (termination-help) for usage.\n")
