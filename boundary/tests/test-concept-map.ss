;;; boundary/test-concept-map.ss --- Tests for ConceptMap Tool
;;;
;;; Verifies concept extraction and relationship inference.
;;;
;;; Run with:
;;;   cd shell && scheme -q < test-concept-map.ss
;;; Or from repl.ss:
;;;   (load "boundary/test-concept-map.ss")

;; Determine the base path based on current directory
(define *test-base-path*
  (if (file-exists? "concept-map.ss")
      "."  ; We're in boundary/
      "shell"))  ; We're in root

;; Load the module under test
(load (string-append *test-base-path* "/concept-map.ss"))

;; Compute path to core/ relative to current directory
(define *core-path*
  (if (file-exists? "concept-map.ss")
      "../core"    ; We're in boundary/
      "core"))     ; We're in root

;;; ====
;;; Test Utilities
;;; ====

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(define (test name expected actual)
  (set! test-count (+ test-count 1))
  (if (equal? expected actual)
      (begin
       (set! pass-count (+ pass-count 1))
       (display (string-append "  PASS: " name "\n")))
      (begin
       (set! fail-count (+ fail-count 1))
       (display (string-append "  FAIL: " name "\n"))
       (display "    Expected: ")
       (write expected)
       (newline)
       (display "    Actual: ")
       (write actual)
       (newline))))

(define (test-pred name pred value)
  (set! test-count (+ test-count 1))
  (if (pred value)
      (begin
       (set! pass-count (+ pass-count 1))
       (display (string-append "  PASS: " name "\n")))
      (begin
       (set! fail-count (+ fail-count 1))
       (display (string-append "  FAIL: " name "\n"))
       (display "    Value did not satisfy predicate: ")
       (write value)
       (newline))))

(define (report-results)
  (newline)
  (display "====\n")
  (display (string-append "RESULTS: " (number->string pass-count) "/"
                          (number->string test-count) " passed"))
  (if (> fail-count 0)
      (display (string-append " (" (number->string fail-count) " failed)"))
      (display " (all passed)"))
  (newline)
  (display "====\n"))

;;; ====
;;; Tests: String Utilities
;;; ====

(display "\n=== String Utilities ===\n")

(test "string-contains? finds substring"
      #t
      (string-contains? "block->bytes" "->"))

(test "string-contains? negative"
      #f
      (string-contains? "hello" "xyz"))

(test "string-prefix? works"
      #t
      (string-prefix? "make-" "make-block"))

(test "string-suffix? works"
      #t
      (string-suffix? "?" "block?"))

(test "string-split-at splits"
      '("block" "bytes")
      (string-split-at "block->bytes" "->"))

(test "string-split by char"
      '("block" "equal" "p")
      (string-split "block-equal-p" #\-))

(test "string-downcase works"
      "hello"
      (string-downcase "HELLO"))

;;; ====
;;; Tests: Symbol Classification
;;; ====

(display "\n=== Symbol Classification ===\n")

(test "capitalized-symbol? true for Block"
      #t
      (capitalized-symbol? 'Block))

(test "capitalized-symbol? false for make-block"
      #f
      (capitalized-symbol? 'make-block))

(test "classify-concept Block -> fundamental"
      'fundamental
      (classify-concept 'Block))

(test "classify-concept Nat -> base"
      'base
      (classify-concept 'Nat))

(test "classify-concept List -> container"
      'container
      (classify-concept 'List))

(test "classify-concept FS -> capability"
      'capability
      (classify-concept 'FS))

;;; ====
;;; Tests: Symbol Collection
;;; ====

(display "\n=== Symbol Collection ===\n")

(test "collect-symbols from simple expr"
      '(a b c)
      (collect-symbols '(a b c)))

(test "collect-symbols from nested expr"
      '(define foo x y)
      (collect-symbols '(define (foo x) y)))

(test "count-occurrences works"
      '((a . 2) (b . 1))
      (let ([counts (count-occurrences '(a b a))])
           (list (assq 'a counts) (assq 'b counts))))

;;; ====
;;; Tests: Definition Extraction
;;; ====

(display "\n=== Definition Extraction ===\n")

;; Test on actual core/block.ss
(let ([defs (extract-definitions (string-append *core-path* "/block.ss"))])
     (test-pred "extract-definitions finds definitions"
                (lambda (x) (> (length x) 0))
                defs)
     
     (test-pred "extract-definitions finds block record"
                (lambda (x)
                        (ormap (lambda (d)
                                       (and (eq? (cdr (assq 'type d)) 'record)
                                            (eq? (cdr (assq 'name d)) 'block)))
                               x))
                defs)
     
     (test-pred "extract-definitions finds make-block function"
                (lambda (x)
                        (ormap (lambda (d)
                                       (and (eq? (cdr (assq 'name d)) 'block->bytes)))
                               x))
                defs))

;;; ====
;;; Tests: Concept Extraction
;;; ====

(display "\n=== Concept Extraction ===\n")

(let ([concepts (extract-concepts (string-append *core-path* "/block.ss"))])
     (test-pred "extract-concepts finds Block"
                (lambda (x)
                        (ormap (lambda (c)
                                       (eq? (cdr (assq 'name c)) 'Block))
                               x))
                concepts)
     
     (test-pred "extract-concepts counts occurrences"
                (lambda (x)
                        (let ([block-concept (find (lambda (c)
                                                           (eq? (cdr (assq 'name c)) 'Block))
                                                   x)])
                             (and block-concept
                                  (> (cdr (assq 'occurrences block-concept)) 0))))
                concepts))

;; Helper for find
(define (find pred lst)
  (cond
   [(null? lst) #f]
   [(pred (car lst)) (car lst)]
   [else (find pred (cdr lst))]))

;;; ====
;;; Tests: Relationship Finding
;;; ====

(display "\n=== Relationship Finding ===\n")

(let* ([defs (extract-definitions (string-append *core-path* "/block.ss"))]
       [rels (find-relationships defs)])
      (test-pred "find-relationships returns list"
                 list?
                 rels)
      
      (test-pred "find-relationships finds field relationships"
                 (lambda (x)
                         (ormap (lambda (r)
                                        (eq? (cadr r) 'has-field))
                                x))
                 rels))

;;; ====
;;; Tests: Graph Building
;;; ====

(display "\n=== Graph Building ===\n")

(let ([graph (build-concept-graph (string-append *core-path* "/block.ss"))])
     (test-pred "build-concept-graph has concepts"
                (lambda (g) (assq 'concepts g))
                graph)
     
     (test-pred "build-concept-graph has relationships"
                (lambda (g) (assq 'relationships g))
                graph)
     
     (test-pred "build-concept-graph has source"
                (lambda (g)
                        (let ([src (assq 'source g)])
                             (and src (string? (cdr src)))))
                graph))

;;; ====
;;; Tests: Rendering
;;; ====

(display "\n=== Rendering ===\n")

(let* ([graph (build-concept-graph (string-append *core-path* "/block.ss"))]
       [output (render-concept-map graph)])
      (test-pred "render-concept-map produces string"
                 string?
                 output)
      
      (test-pred "render-concept-map contains header"
                 (lambda (s) (string-contains? s "CONCEPT MAP"))
                 output)
      
      (test-pred "render-concept-map contains source path"
                 (lambda (s) (string-contains? s "block.ss"))
                 output))

;;; ====
;;; Tests: Full Integration
;;; ====

(display "\n=== Full Integration ===\n")

;; Test on types.ss as well
(let ([graph (build-concept-graph (string-append *core-path* "/types.ss"))])
     (test-pred "types.ss graph has Type concept"
                (lambda (g)
                        (let ([concepts (cdr (assq 'concepts g))])
                             (assq 'Type concepts)))
                graph))

;;; ====
;;; Demo Output
;;; ====

(display "\n=== Demo: Concept Map for core/block.ss ===\n")
(let ([graph (concept-map (string-append *core-path* "/block.ss"))])
     (void))

(display "\n=== Demo: Concept Map for core/types.ss ===\n")
(let ([graph (concept-map (string-append *core-path* "/types.ss"))])
     (void))

;;; ====
;;; Report
;;; ====

(report-results)

;; Exit with appropriate code (only when running as script)
;; Comment out for interactive use
(when (not (terminal-port? (current-input-port)))
      (if (> fail-count 0)
          (exit 1)
          (exit 0)))
