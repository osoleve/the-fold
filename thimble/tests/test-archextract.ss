;;; Test harness for shell/archextract.ss

;;; Load dependencies from core
(load "fabric/stitches/prelude.ss")

;;; Load the archextract module
(load "thimble/archextract.ss")

;;; ============================================================
;;; Test Framework
;;; ============================================================

(define *tests-passed* 0)
(define *tests-failed* 0)

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (begin
        (set! *tests-passed* (+ *tests-passed* 1))
        (display "PASS"))
      (begin
        (set! *tests-failed* (+ *tests-failed* 1))
        (display "FAIL\n    expected: ")
        (write expected)
        (display "\n    got: ")
        (write actual)))
  (newline))

(define (test-true name actual)
  (test name #t actual))

(define (test-false name actual)
  (test name #f actual))

(define (test-summary)
  (newline)
  (display "============================================================\n")
  (display "SUMMARY: ")
  (display *tests-passed*)
  (display " passed, ")
  (display *tests-failed*)
  (display " failed\n")
  (display "============================================================\n"))

;;; ============================================================
;;; Test Path Utilities
;;; ============================================================

(display "\n")
(display "============================================================\n")
(display "  ARCHEXTRACT TESTS\n")
(display "============================================================\n\n")

(display "Test 1: Path utilities\n")
(test "basename simple" "foo.ss" (arch-path-basename "foo.ss"))
(test "basename with dir" "bar.ss" (arch-path-basename "foo/bar.ss"))
(test "basename with backslash" "bar.ss" (arch-path-basename "foo\\bar.ss"))
(test "dirname simple" "foo" (arch-path-dirname "foo/bar.ss"))
(test "dirname dot" "." (arch-path-dirname "bar.ss"))
(test "stem simple" "foo" (arch-path-stem "foo.ss"))
(test "stem no ext" "foo" (arch-path-stem "foo"))
(test "suffix true" #t (arch-string-suffix? ".ss" "foo.ss"))
(test "suffix false" #f (arch-string-suffix? ".ss" "foo.txt"))

;;; ============================================================
;;; Test Path Resolution
;;; ============================================================

(display "\nTest 2: Path resolution\n")
(test "normalize path" "foo/bar/baz" (normalize-path-sep "foo\\bar\\baz"))
(test "split path" '("foo" "bar" "baz") (split-path "foo/bar/baz"))
(test "join path" "foo/bar/baz" (join-path '("foo" "bar" "baz")))
(test "join empty" "" (join-path '()))
(test "resolve parts simple" '("foo" "bar" "file.ss")
      (resolve-parts '("foo" "bar") '("file.ss")))
(test "resolve parts .." '("foo" "other.ss")
      (resolve-parts '("foo" "bar") '(".." "other.ss")))

;;; ============================================================
;;; Test Load Extraction
;;; ============================================================

(display "\nTest 3: Extract loads\n")
(test "extract single load" '("prelude.ss")
      (extract-loads '((load "prelude.ss") (define x 1))))
(test "extract multiple loads" '("a.ss" "b.ss")
      (extract-loads '((load "a.ss") (define x 1) (load "b.ss"))))
(test "extract no loads" '()
      (extract-loads '((define x 1) (define y 2))))
(test "extract nested load" '("nested.ss")
      (extract-loads '((when something (load "nested.ss")))))

;;; ============================================================
;;; Test Define Extraction
;;; ============================================================

(display "\nTest 4: Extract defines\n")
(test "extract simple define" '(x)
      (extract-defines '((define x 1))))
(test "extract function define" '(foo)
      (extract-defines '((define (foo x) x))))
(test "extract multiple defines" '(a b c)
      (extract-defines '((define a 1) (define (b x) x) (define c 3))))
(test "extract define-record-type" '(my-record)
      (extract-defines '((define-record-type my-record (fields x y)))))

;;; ============================================================
;;; Test Module Analysis
;;; ============================================================

(display "\nTest 5: Analyze actual module (fabric/stitches/prelude.ss)\n")
(let ([mod (analyze-module "fabric/stitches/prelude.ss")])
  (test-true "module analyzed" (and mod #t))
  (test "module name" "prelude" (cdr (assq 'name mod)))
  ;; prelude.ss has no loads
  (test "prelude loads" '() (cdr (assq 'loads mod)))
  ;; prelude.ss defines many functions
  (test-true "prelude has defines" (> (length (cdr (assq 'defines mod))) 5)))

(display "\nTest 6: Analyze module with loads (fabric/stitches/block.ss)\n")
(let ([mod (analyze-module "fabric/stitches/block.ss")])
  (test-true "module analyzed" (and mod #t))
  (test "module name" "block" (cdr (assq 'name mod)))
  ;; block.ss loads prelude.ss
  (test "block loads prelude" '("prelude.ss") (cdr (assq 'loads mod))))

;;; ============================================================
;;; Test Directory Analysis
;;; ============================================================

(display "\nTest 7: Analyze directory (fabric/stitches/)\n")
(let ([modules (analyze-directory "fabric/stitches")])
  (test-true "found modules" (> (length modules) 5))
  (test-true "found prelude" (ormap (lambda (m) (string=? "prelude" (cdr (assq 'name m)))) modules))
  (test-true "found block" (ormap (lambda (m) (string=? "block" (cdr (assq 'name m)))) modules)))

;;; ============================================================
;;; Test Graph Building
;;; ============================================================

(display "\nTest 8: Build dependency graph\n")
(let* ([modules (analyze-directory "fabric/stitches")]
       [graph (build-dependency-graph modules)])
  (let ([nodes (cdr (assq 'nodes graph))]
        [edges (cdr (assq 'edges graph))])
    (test-true "has nodes" (> (length nodes) 5))
    (test-true "has edges" (> (length edges) 0))
    ;; block depends on prelude
    (test-true "block->prelude edge"
               (ormap (lambda (e) (and (string=? (car e) "block")
                                       (string=? (cdr e) "prelude")))
                      edges))))

;;; ============================================================
;;; Test Cycle Detection
;;; ============================================================

(display "\nTest 9: Cycle detection\n")
;; Create a test graph with a cycle: a -> b -> c -> a
(let* ([nodes '("a" "b" "c")]
       [edges '(("a" . "b") ("b" . "c") ("c" . "a"))]
       [graph `((nodes . ,nodes) (edges . ,edges))]
       [cycles (find-cycles graph)])
  (test-true "found cycle" (> (length cycles) 0)))

;; Create a test graph without cycles: a -> b -> c
(let* ([nodes '("a" "b" "c")]
       [edges '(("a" . "b") ("b" . "c"))]
       [graph `((nodes . ,nodes) (edges . ,edges))]
       [cycles (find-cycles graph)])
  (test "no cycles in DAG" '() cycles))

;;; ============================================================
;;; Test Critical Modules
;;; ============================================================

(display "\nTest 10: Critical modules\n")
(let* ([nodes '("a" "b" "c")]
       [edges '(("a" . "c") ("b" . "c"))]  ; c has 2 dependents
       [graph `((nodes . ,nodes) (edges . ,edges))]
       [critical (critical-modules graph)])
  (test "most critical" "c" (caar critical))
  (test "c has 2 dependents" 2 (cdar critical)))

;;; ============================================================
;;; Test ASCII Rendering
;;; ============================================================

(display "\nTest 11: ASCII rendering\n")
(let* ([nodes '("foo" "bar")]
       [edges '(("foo" . "bar"))]
       [graph `((nodes . ,nodes) (edges . ,edges))]
       [output (render-graph-ascii graph)])
  (test-true "output is string" (string? output))
  (test-true "contains foo" (string-contains output "foo"))
  (test-true "contains bar" (string-contains output "bar"))
  (test-true "contains arrow" (string-contains output "-->")))

;;; ============================================================
;;; Integration Test: Full Report
;;; ============================================================

(display "\nTest 12: Full report generation\n")
(let ([report (archextract-report "fabric/stitches")])
  (test-true "report is string" (string? report))
  (test-true "report has header" (string-contains report "ARCHITECTURE REPORT"))
  (test-true "report has modules" (string-contains report "MODULES"))
  (test-true "report has graph" (string-contains report "DEPENDENCY GRAPH"))
  (test-true "report has cycles section" (string-contains report "CYCLES"))
  (test-true "report has critical section" (string-contains report "CRITICAL MODULES")))

;;; ============================================================
;;; Summary
;;; ============================================================

(test-summary)

;;; Show sample output
(display "\n\nSAMPLE OUTPUT: fabric/stitches/ directory analysis\n")
(display "============================================================\n\n")
(archextract "fabric/stitches")
