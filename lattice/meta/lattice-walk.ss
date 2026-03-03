;;; lattice/meta/lattice-walk.ss — Dependency-order lattice analysis walker
;;; @module meta/lattice-walk
;;; @requires prelude

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)

(doc 'module 'meta/lattice-walk)
(doc 'description "Pure topsort walker and pluggable analyzer protocol.
Processes modules in dependency order, threading accumulated state through
a sequence of analyzers. Each analyzer sees its dependencies' results before
its own module is processed.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================================
;;; Module Records
;;; ============================================================================

(doc 'section 'module-records)

;;; ModuleRecord: pre-parsed module data for analysis (no I/O).
;;; Created by the boundary driver from file reads.

(doc make-module-record 'export #t)
(doc make-module-record 'type '(-> Symbol String (List Symbol) (List SExp) String ModuleRecord))
(doc make-module-record 'description "Create a module record from pre-read data.")
(define (make-module-record name path deps sexps fingerprint)
  (vector 'module-record name path deps sexps fingerprint))

(doc module-record? 'export #t)
(define (module-record? x)
  (and (vector? x)
       (= (vector-length x) 6)
       (eq? (vector-ref x 0) 'module-record)))

(doc module-record-name 'export #t)
(define (module-record-name mr) (vector-ref mr 1))

(doc module-record-path 'export #t)
(define (module-record-path mr) (vector-ref mr 2))

(doc module-record-deps 'export #t)
(define (module-record-deps mr) (vector-ref mr 3))

(doc module-record-sexps 'export #t)
(define (module-record-sexps mr) (vector-ref mr 4))

(doc module-record-fingerprint 'export #t)
(define (module-record-fingerprint mr) (vector-ref mr 5))

;;; ============================================================================
;;; Analyzer Protocol
;;; ============================================================================

(doc 'section 'analyzers)

;;; An Analyzer is a pluggable analysis pass.
;;;   key:     Symbol identifying this analyzer in the accumulator
;;;   init-fn: () -> AnalyzerState — initial state
;;;   step-fn: (ModuleRecord AccState AnalyzerState) -> AnalyzerState
;;;            Process one module. Receives the full AccState for cross-reads.

(doc make-analyzer 'export #t)
(doc make-analyzer 'type '(-> Symbol (-> AnalyzerState) (-> ModuleRecord AccState AnalyzerState AnalyzerState) Analyzer))
(doc make-analyzer 'description "Create an analyzer with key, init function, and step function.")
(define (make-analyzer key init-fn step-fn)
  (vector 'analyzer key init-fn step-fn))

(doc analyzer? 'export #t)
(define (analyzer? x)
  (and (vector? x)
       (= (vector-length x) 4)
       (eq? (vector-ref x 0) 'analyzer)))

(doc analyzer-key 'export #t)
(define (analyzer-key a) (vector-ref a 1))

(doc analyzer-init 'export #t)
(define (analyzer-init a) (vector-ref a 2))

(doc analyzer-step 'export #t)
(define (analyzer-step a) (vector-ref a 3))

;;; ============================================================================
;;; Topological Sort (Kahn's Algorithm)
;;; ============================================================================

(doc 'section 'topsort)

;;; topo-sort-modules : (List ModuleRecord) -> (Result (List ModuleRecord) (List Symbol))
;;; Sort modules so dependencies come before dependents.
;;; External deps (not in the input set) are ignored — they're already available.
;;; Returns (ok sorted-list) or (err (cycle module-names...)) if a cycle is detected.
(doc topo-sort-modules 'export #t)
(doc topo-sort-modules 'type '(-> (List ModuleRecord) (Result (List ModuleRecord) (List Symbol))))
(define (topo-sort-modules modules)
  (let* ([name->mr (make-eq-hashtable)]
         [in-degree (make-eq-hashtable)]
         [graph (make-eq-hashtable)]      ; dep -> (list of dependents)
         [names '()])
    ;; Index modules by name
    (for-each (lambda (mr)
                (let ([n (module-record-name mr)])
                  (hashtable-set! name->mr n mr)
                  (hashtable-set! in-degree n 0)
                  (hashtable-set! graph n '())
                  (set! names (cons n names))))
              modules)
    ;; Build edges: for each module, its deps point to it
    (for-each
     (lambda (mr)
       (let ([n (module-record-name mr)])
         (for-each
          (lambda (dep)
            ;; Only count edges within the input set
            (when (hashtable-ref name->mr dep #f)
              (hashtable-set! graph dep
                              (cons n (hashtable-ref graph dep '())))
              (hashtable-set! in-degree n
                              (+ 1 (hashtable-ref in-degree n 0)))))
          (module-record-deps mr))))
     modules)
    ;; Kahn's: start with zero-in-degree nodes
    (let ([queue (filter (lambda (n) (zero? (hashtable-ref in-degree n 0))) names)])
      (let loop ([q queue] [result '()] [count 0])
        (if (null? q)
            ;; Done — check if all modules were visited
            (if (= count (length modules))
                (list 'ok (reverse result))
                ;; Cycle detected: remaining modules with nonzero in-degree
                (let ([stuck (filter (lambda (n)
                                       (> (hashtable-ref in-degree n 0) 0))
                                     names)])
                  (list 'err stuck)))
            (let* ([node (car q)]
                   [rest-q (cdr q)]
                   [neighbors (hashtable-ref graph node '())]
                   [new-q
                    (fold-left
                     (lambda (acc neighbor)
                       (let ([new-deg (- (hashtable-ref in-degree neighbor 0) 1)])
                         (hashtable-set! in-degree neighbor new-deg)
                         (if (zero? new-deg)
                             (cons neighbor acc)
                             acc)))
                     rest-q
                     neighbors)])
              (loop new-q
                    (cons (hashtable-ref name->mr node #f) result)
                    (+ count 1))))))))

;;; ============================================================================
;;; Walker
;;; ============================================================================

(doc 'section 'walker)

;;; acc-ref : AccState Symbol -> AnalyzerState
;;; Look up an analyzer's state in the accumulator.
(doc acc-ref 'export #t)
(define (acc-ref acc key)
  (let ([entry (assq key acc)])
    (if entry (cdr entry) #f)))

;;; acc-set : AccState Symbol AnalyzerState -> AccState
;;; Replace an analyzer's state in the accumulator.
(define (acc-set acc key new-state)
  (map (lambda (entry)
         (if (eq? (car entry) key)
             (cons key new-state)
             entry))
       acc))

;;; lattice-walk : (List ModuleRecord) (List Analyzer) -> WalkResult
;;; Process modules in dependency order, threading analyzer state.
;;;
;;; Analyzers run sequentially within each module step (in list order).
;;; This means later analyzers can read earlier analyzers' updated state
;;; for the same module — critical for unused-requires reading exports.
;;;
;;; Returns:
;;;   ((ok . #t)
;;;    (modules-processed . N)
;;;    (state . final-accumulated-state))
;;; Or on topsort failure:
;;;   ((ok . #f)
;;;    (error . "cycle detected")
;;;    (cycle . (module-names...)))
(doc lattice-walk 'export #t)
(doc lattice-walk 'type '(-> (List ModuleRecord) (List Analyzer) WalkResult))
(define (lattice-walk module-records analyzers)
  (let ([sort-result (topo-sort-modules module-records)])
    (if (eq? (car sort-result) 'err)
        ;; Cycle detected
        `((ok . #f)
          (error . "cycle detected")
          (cycle . ,(cadr sort-result)))
        ;; Walk in topsort order
        (let* ([sorted (cadr sort-result)]
               ;; Initialize accumulator
               [init-acc (map (lambda (a)
                                (cons (analyzer-key a) ((analyzer-init a))))
                              analyzers)]
               ;; Fold over modules
               [final-acc
                (fold-left
                 (lambda (acc mr)
                   ;; Run each analyzer sequentially on this module
                   (fold-left
                    (lambda (current-acc analyzer)
                      (let* ([key (analyzer-key analyzer)]
                             [step (analyzer-step analyzer)]
                             [current-state (acc-ref current-acc key)]
                             [new-state (step mr current-acc current-state)])
                        (acc-set current-acc key new-state)))
                    acc
                    analyzers))
                 init-acc
                 sorted)])
          `((ok . #t)
            (modules-processed . ,(length sorted))
            (state . ,final-acc))))))

;;; walk-state : WalkResult -> AccState | #f
;;; Extract accumulated state from a successful walk result.
(doc walk-state 'export #t)
(define (walk-state result)
  (let ([entry (assq 'state result)])
    (if entry (cdr entry) #f)))

;;; walk-ok? : WalkResult -> Boolean
;;; Check if walk completed successfully.
(doc walk-ok? 'export #t)
(define (walk-ok? result)
  (let ([entry (assq 'ok result)])
    (and entry (cdr entry))))
