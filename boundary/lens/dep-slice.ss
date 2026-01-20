(load "core/base/prelude.ss")

(doc 'module 'dep-slice)
(doc 'description "Dependency slicing for transitive closure computation")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'section 'transitive-closure)

;;; compute-closure : Symbol × (Symbol -> (List Symbol)) × Nat -> (List Symbol)
;;; Compute transitive closure using the given neighbor function.
;;; max-depth limits the search depth (default 10).
(define (compute-closure start get-neighbors max-depth)
  (let ([visited (make-eq-hashtable)]
        [result '()])
       
       (define (visit sym depth)
         (unless (or (hashtable-ref visited sym #f)
                     (> depth max-depth))
                 (hashtable-set! visited sym #t)
                 (set! result (cons sym result))
                 (for-each
                  (lambda (neighbor)
                          (visit neighbor (+ depth 1)))
                  (get-neighbors sym))))
       
       ;; Start traversal from each immediate neighbor
       (for-each
        (lambda (neighbor)
                (visit neighbor 1))
        (get-neighbors start))
       
       (reverse result)))

;;; ====
;;; Public API
;;; ====

;;; dep-slice-up : Symbol [Nat] -> (List Symbol)
;;; Find all symbols that transitively depend on sym.
;;; These are the "upward" dependencies - who uses this?
(define (dep-slice-up sym . opts)
  (let ([max-depth (if (null? opts) 10 (car opts))])
       (compute-closure sym call-graph-callers max-depth)))

;;; dep-slice-down : Symbol [Nat] -> (List Symbol)
;;; Find all symbols that sym transitively depends on.
;;; These are the "downward" dependencies - what does this need?
(define (dep-slice-down sym . opts)
  (let ([max-depth (if (null? opts) 10 (car opts))])
       (compute-closure sym call-graph-callees max-depth)))

;;; dep-slice-both : Symbol [Nat] -> ((up . (List Symbol)) (down . (List Symbol)))
;;; Compute both upward and downward slices.
(define (dep-slice-both sym . opts)
  (let ([max-depth (if (null? opts) 10 (car opts))])
       `((up . ,(dep-slice-up sym max-depth))
         (down . ,(dep-slice-down sym max-depth)))))

;;; ====
;;; Layered Slicing (by depth)
;;; ====

;;; dep-slice-layers-up : Symbol Nat -> (List (depth . (List Symbol)))
;;; Return dependents organized by distance from symbol.
(define (dep-slice-layers-up sym max-depth)
  (let ([layers (make-vector (+ max-depth 1) '())]
        [visited (make-eq-hashtable)])
       
       (define (visit sym depth)
         (unless (or (hashtable-ref visited sym #f)
                     (> depth max-depth))
                 (hashtable-set! visited sym #t)
                 (vector-set! layers depth
                              (cons sym (vector-ref layers depth)))
                 (for-each
                  (lambda (neighbor)
                          (visit neighbor (+ depth 1)))
                  (call-graph-callers sym))))
       
       ;; Start from immediate callers
       (for-each
        (lambda (caller)
                (visit caller 1))
        (call-graph-callers sym))
       
       ;; Convert to alist
       (let loop ([i 1] [result '()])
            (if (> i max-depth)
                (reverse result)
                (let ([layer (vector-ref layers i)])
                     (if (null? layer)
                         (loop (+ i 1) result)
                         (loop (+ i 1)
                               (cons (cons i (reverse layer)) result))))))))

;;; dep-slice-layers-down : Symbol Nat -> (List (depth . (List Symbol)))
;;; Return dependencies organized by distance from symbol.
(define (dep-slice-layers-down sym max-depth)
  (let ([layers (make-vector (+ max-depth 1) '())]
        [visited (make-eq-hashtable)])
       
       (define (visit sym depth)
         (unless (or (hashtable-ref visited sym #f)
                     (> depth max-depth))
                 (hashtable-set! visited sym #t)
                 (vector-set! layers depth
                              (cons sym (vector-ref layers depth)))
                 (for-each
                  (lambda (neighbor)
                          (visit neighbor (+ depth 1)))
                  (call-graph-callees sym))))
       
       ;; Start from immediate callees
       (for-each
        (lambda (callee)
                (visit callee 1))
        (call-graph-callees sym))
       
       ;; Convert to alist
       (let loop ([i 1] [result '()])
            (if (> i max-depth)
                (reverse result)
                (let ([layer (vector-ref layers i)])
                     (if (null? layer)
                         (loop (+ i 1) result)
                         (loop (+ i 1)
                               (cons (cons i (reverse layer)) result))))))))

;;; ====
;;; Impact Analysis
;;; ====

;;; dep-impact : Symbol -> (impact-score direct-deps transitive-deps)
;;; Compute the "impact" of changing a symbol.
;;; Higher score = more things depend on this.
(define (dep-impact sym)
  (let* ([direct (call-graph-callers sym)]
         [transitive (dep-slice-up sym)]
         [direct-count (length direct)]
         [transitive-count (length transitive)])
        `((symbol . ,sym)
          (direct-dependents . ,direct-count)
          (transitive-dependents . ,transitive-count)
          (impact-score . ,(+ (* direct-count 2) transitive-count)))))

;;; dep-complexity : Symbol -> (complexity-score direct-deps transitive-deps)
;;; Compute the "complexity" of a symbol.
;;; Higher score = depends on more things.
(define (dep-complexity sym)
  (let* ([direct (call-graph-callees sym)]
         [transitive (dep-slice-down sym)]
         [direct-count (length direct)]
         [transitive-count (length transitive)])
        `((symbol . ,sym)
          (direct-dependencies . ,direct-count)
          (transitive-dependencies . ,transitive-count)
          (complexity-score . ,(+ (* direct-count 2) transitive-count)))))

;;; ====
;;; Display Functions
;;; ====

;;; dep-slice-stats : Symbol -> void
;;; Display dependency statistics for a symbol.
(define (dep-slice-stats sym)
  (unless (call-graph-built?)
          (call-graph-refresh!))
  
  (let* ([direct-up (call-graph-callers sym)]
         [direct-down (call-graph-callees sym)]
         [trans-up (dep-slice-up sym)]
         [trans-down (dep-slice-down sym)]
         [impact (dep-impact sym)]
         [complexity (dep-complexity sym)])
        
        (display "\n")
        (printf "  Dependency Analysis: ~a\n" sym)
        (display "  ────────────────────────────────\n")
        (display "\n")
        (display "  Dependents (who uses this?):\n")
        (printf "    Direct:     ~a\n" (length direct-up))
        (printf "    Transitive: ~a\n" (length trans-up))
        (display "\n")
        (display "  Dependencies (what does this use?):\n")
        (printf "    Direct:     ~a\n" (length direct-down))
        (printf "    Transitive: ~a\n" (length trans-down))
        (display "\n")
        (printf "  Impact Score:     ~a\n" (cdr (assq 'impact-score impact)))
        (printf "  Complexity Score: ~a\n" (cdr (assq 'complexity-score complexity)))
        (display "\n")))

(display "Dependency slicing module loaded.\n")
