;;; fabric/stitches/profile.ss — Inline Performance Profiler
;;;
;;; Tracks fuel consumption, call counts, and timing during evaluation.
;;; Builds hierarchical call trees for performance analysis.
;;;
;;; This is Core code: pure profiling data collection.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - eval.ss

(load "core/base/prelude.ss")
(load "core/lang/eval.ss")

;;; ============================================================
;;; Call Tree Data Structures
;;; ============================================================

;;; make-node : Symbol × Nat → Node
(define (make-node name start-fuel)
  `(node
    (name . ,name)
    (start-fuel . ,start-fuel)
    (end-fuel . #f)
    (children . ())
    (call-count . 1)
    (time-ns . #f)))

(define (node? n)
  (and (pair? n) (eq? (car n) 'node)))

(define (node-get n key)
  (let ([entry (assq key (cdr n))])
       (and entry (cdr entry))))

(define (node-name n) (node-get n 'name))
(define (node-start-fuel n) (node-get n 'start-fuel))
(define (node-end-fuel n) (node-get n 'end-fuel))
(define (node-children n) (node-get n 'children))
(define (node-call-count n) (node-get n 'call-count))

(define (node-fuel-consumed n)
  (let ([start (node-start-fuel n)]
        [end (node-end-fuel n)])
       (if (and start end)
           (- start end)
           0)))

(define (node-set n key value)
  (cons 'node
        (map (lambda (entry)
                     (if (eq? (car entry) key)
                         (cons key value)
                         entry))
             (cdr n))))

(define (node-add-child n child)
  (node-set n 'children (cons child (node-children n))))

(define (node-close n end-fuel)
  (node-set n 'end-fuel end-fuel))

(define (node-increment-calls n)
  (node-set n 'call-count (+ 1 (node-call-count n))))

;;; ============================================================
;;; Expression Classification
;;; ============================================================

(define (expr-name expr)
  (cond
   [(symbol? expr) expr]
   [(not (pair? expr)) 'literal]
   [else
    (let ([head (car expr)])
         (cond
          [(eq? head 'fn) 'lambda]
          [(eq? head 'fix)
           (if (and (pair? (cdr expr)) (symbol? (cadr expr)))
               (cadr expr)
               'fix)]
          [(eq? head 'quote) 'quote]
          [(eq? head 'let) 'let]
          [(eq? head 'if) 'if]
          [(eq? head 'case) 'case]
          [(eq? head 'prim)
           (if (and (pair? (cdr expr))
                    (pair? (cadr expr))
                    (eq? (car (cadr expr)) 'quote))
               (cadr (cadr expr))
               'prim)]
          [(eq? head 'call)
           (if (and (pair? (cdr expr)) (symbol? (cadr expr)))
               (cadr expr)
               'call)]
          [(symbol? head) head]
          [else 'expr]))]))

;;; ============================================================
;;; Profiler State
;;; ============================================================

;;; make-profiler : Expr × Env × Nat → Profiler
(define (make-profiler expr env fuel)
  `(profiler
    (expr . ,expr)
    (env . ,env)
    (initial-fuel . ,fuel)
    (root . ,(make-node 'root fuel))
    (status . ready)
    (result . #f)))

(define (profiler? p)
  (and (pair? p) (eq? (car p) 'profiler)))

(define (profiler-get p key)
  (let ([entry (assq key (cdr p))])
       (and entry (cdr entry))))

(define (profiler-expr p) (profiler-get p 'expr))
(define (profiler-env p) (profiler-get p 'env))
(define (profiler-initial-fuel p) (profiler-get p 'initial-fuel))
(define (profiler-root p) (profiler-get p 'root))
(define (profiler-status p) (profiler-get p 'status))
(define (profiler-result p) (profiler-get p 'result))

(define (profiler-set p key value)
  (cons 'profiler
        (map (lambda (entry)
                     (if (eq? (car entry) key)
                         (cons key value)
                         entry))
             (cdr p))))

(define (profiler-update p updates)
  (fold-left (lambda (p update)
                     (profiler-set p (car update) (cdr update)))
             p
             updates))

;;; ============================================================
;;; Simple Profiling (Run to Completion)
;;; ============================================================

;;; profile-expr : Expr × Fuel → Profiler
;;; Profile an expression with given fuel budget (simple approach)
(define (profile-expr expr fuel)
  (let* ([env empty-env]
         [result (eval-expr expr env fuel)])
        (cond
         [(eq? (car result) 'ok)
          (let* ([value (cadr result)]
                 [remaining (caddr result)]
                 [used (- fuel remaining)]
                 [root (node-close (make-node (expr-name expr) fuel) remaining)])
                `(profiler
                  (expr . ,value)
                  (env . ,env)
                  (initial-fuel . ,fuel)
                  (root . ,root)
                  (status . complete)
                  (result . ,value)))]
         [(eq? (car result) 'suspended)
          `(profiler
            (expr . ,(cadr result))
            (env . ,(caddr result))
            (initial-fuel . ,fuel)
            (root . ,(make-node (expr-name expr) fuel))
            (status . suspended)
            (result . #f))]
         [else
          `(profiler
            (expr . ,expr)
            (env . ,env)
            (initial-fuel . ,fuel)
            (root . ,(make-node (expr-name expr) fuel))
            (status . error)
            (result . ,result))])))

;;; profile-with-env : Expr × Env × Fuel → Profiler
(define (profile-with-env expr env fuel)
  (let* ([result (eval-expr expr env fuel)])
        (cond
         [(eq? (car result) 'ok)
          (let* ([value (cadr result)]
                 [remaining (caddr result)]
                 [root (node-close (make-node (expr-name expr) fuel) remaining)])
                `(profiler
                  (expr . ,value)
                  (env . ,env)
                  (initial-fuel . ,fuel)
                  (root . ,root)
                  (status . complete)
                  (result . ,value)))]
         [(eq? (car result) 'suspended)
          `(profiler
            (expr . ,(cadr result))
            (env . ,(caddr result))
            (initial-fuel . ,fuel)
            (root . ,(make-node (expr-name expr) fuel))
            (status . suspended)
            (result . #f))]
         [else
          `(profiler
            (expr . ,expr)
            (env . ,env)
            (initial-fuel . ,fuel)
            (root . ,(make-node (expr-name expr) fuel))
            (status . error)
            (result . ,result))])))

;;; ============================================================
;;; Statistics Extraction
;;; ============================================================

;;; flatten-tree : Node → List of (name . fuel-consumed)
(define (flatten-tree node)
  (let ([self (cons (node-name node) (node-fuel-consumed node))]
        [children (node-children node)])
       (cons self
             (apply append (map flatten-tree children)))))

;;; aggregate-by-name : List of (name . fuel) → Alist of (name . (total-fuel . count))
(define (aggregate-by-name calls)
  (let loop ([calls calls] [acc '()])
       (if (null? calls)
           acc
           (let* ([call (car calls)]
                  [name (car call)]
                  [fuel (cdr call)]
                  [existing (assq name acc)])
                 (if existing
                     (loop (cdr calls)
                           (map (lambda (e)
                                        (if (eq? (car e) name)
                                            (cons name
                                                  (cons (+ fuel (cadr e))
                                                        (+ 1 (cddr e))))
                                            e))
                                acc))
                     (loop (cdr calls)
                           (cons (cons name (cons fuel 1)) acc)))))))

;;; profile-stats : Profiler → Alist
(define (profile-stats p)
  (let* ([root (profiler-root p)]
         [total-fuel (profiler-initial-fuel p)]
         [used-fuel (node-fuel-consumed root)]
         [flat (flatten-tree root)]
         [by-name (aggregate-by-name flat)])
        `((total-fuel . ,total-fuel)
          (used-fuel . ,used-fuel)
          (efficiency . ,(if (zero? total-fuel) 0
                             (/ used-fuel total-fuel)))
          (by-function . ,by-name))))

;;; ============================================================
;;; Hotspot Analysis
;;; ============================================================

;;; find-hotspots : Profiler × Nat → List of (name fuel% calls)
(define (find-hotspots p n)
  (let* ([stats (profile-stats p)]
         [by-fn (cdr (assq 'by-function stats))]
         [total (cdr (assq 'used-fuel stats))]
         [sorted (list-sort (lambda (a b)
                                    (> (cadr a) (cadr b)))
                            by-fn)]
         [top (take-up-to n sorted)])
        (map (lambda (entry)
                     (let ([name (car entry)]
                           [fuel (cadr entry)]
                           [calls (cddr entry)])
                          (list name
                                (if (zero? total) 0
                                    (* 100.0 (/ fuel total)))
                                calls)))
             top)))

(define (take-up-to n lst)
  (if (or (zero? n) (null? lst))
      '()
      (cons (car lst) (take-up-to (- n 1) (cdr lst)))))

;;; ============================================================
;;; Call Tree Rendering
;;; ============================================================

;;; render-tree : Node × Nat → String
(define (render-tree node depth)
  (let* ([indent (make-string (* depth 2) #\space)]
         [name (node-name node)]
         [fuel (node-fuel-consumed node)]
         [calls (node-call-count node)]
         [children (reverse (node-children node))]
         [header (format "~a~a: ~a fuel (~a calls)\n"
                         indent name fuel calls)]
         [child-strs (map (lambda (c) (render-tree c (+ depth 1)))
                          children)])
        (apply string-append (cons header child-strs))))

;;; ============================================================
;;; Path Analysis
;;; ============================================================

(define (collect-paths node prefix)
  (let* ([current (cons (cons (node-name node) (node-fuel-consumed node))
                        prefix)]
         [children (node-children node)])
        (if (null? children)
            (list (reverse current))
            (apply append
                   (map (lambda (c) (collect-paths c current))
                        children)))))

(define (path-fuel path)
  (fold-left (lambda (acc step) (+ acc (cdr step)))
             0
             path))

;;; render-path : Path → String
(define (render-path path)
  (let* ([names (map car path)]
         [total (path-fuel path)]
         [arrow-str (let loop ([ns names])
                         (if (null? (cdr ns))
                             (symbol->string (car ns))
                             (string-append (symbol->string (car ns))
                                            " -> "
                                            (loop (cdr ns)))))])
        (format "~a (~a fuel)" arrow-str total)))

;;; find-hot-paths : Node × Nat → (List Path)
(define (find-hot-paths root n)
  (let* ([all-paths (collect-paths root '())]
         [sorted (list-sort (lambda (a b)
                                    (> (path-fuel a) (path-fuel b)))
                            all-paths)])
        (take-up-to n sorted)))

;;; ============================================================
;;; Public API
;;; ============================================================

;;; profile : Expr × Nat → Profiler
(define (profile expr fuel)
  (profile-expr expr fuel))

;;; profile-report : Profiler → Void
(define (profile-report p)
  (let* ([stats (profile-stats p)]
         [total (cdr (assq 'total-fuel stats))]
         [used (cdr (assq 'used-fuel stats))]
         [eff (cdr (assq 'efficiency stats))]
         [hotspots (find-hotspots p 10)]
         [root (profiler-root p)]
         [hot-paths (find-hot-paths root 5)])
        
        (display "\n")
        (display "  ============================================\n")
        (display "              PROFILE REPORT\n")
        (display "  ============================================\n\n")
        
        (display (format "  Fuel Budget:    ~a\n" total))
        (display (format "  Fuel Used:      ~a\n" used))
        (display (format "  Efficiency:     ~a%\n\n" (round (* 100 eff))))
        
        (display "  --- Top Functions by Fuel ---\n\n")
        (for-each
         (lambda (h)
                 (display (format "    ~a: ~a% (~a calls)\n"
                                  (car h)
                                  (round (cadr h))
                                  (caddr h))))
         hotspots)
        
        (display "\n  --- Hot Paths ---\n\n")
        (for-each
         (lambda (path)
                 (display (format "    ~a\n" (render-path path))))
         hot-paths)
        
        (display "\n  --- Call Tree ---\n\n")
        (display (render-tree root 2))
        (display "\n")))
