;;; core/geometry/test-obj-loader.ss — Tests for OBJ File Loader
;;;
;;; Tests for OBJ parsing, vertex extraction, and mesh generation.
;;;
;;; Run: scheme --script core/geometry/test-obj-loader.ss

(load "core/test-framework.ss")
(load "lattice/geometry/obj-loader.ss")

(set! *current-group* 'obj-loader)

;;; Helper for approximate equality
(define (approx= a b eps)
  (< (abs (- a b)) eps))

;;; Helper for approximate vector equality
(define (vec3-approx= v1 v2 eps)
  (and (approx= (vec3-x v1) (vec3-x v2) eps)
       (approx= (vec3-y v1) (vec3-y v2) eps)
       (approx= (vec3-z v1) (vec3-z v2) eps)))

;;; ============================================================
;;; String Utilities Tests
;;; ============================================================

(define-test "string-split basic"
  (let ([parts (string-split "a b c" #\space)])
       (assert-equal (length parts) 3)
       (assert-equal (car parts) "a")
       (assert-equal (cadr parts) "b")
       (assert-equal (caddr parts) "c")))

(define-test "string-split empty string"
  (let ([parts (string-split "" #\space)])
       (assert-equal (length parts) 0)))

(define-test "string-split no delimiter"
  (let ([parts (string-split "hello" #\space)])
       (assert-equal (length parts) 1)
       (assert-equal (car parts) "hello")))

(define-test "string-split multiple delimiters"
  (let ([parts (string-split "a  b" #\space)])
       ;; Multiple spaces should produce empty strings between
       (assert-true (>= (length parts) 2))))

(define-test "string-trim basic"
  (let ([result (string-trim "  hello  ")])
       (assert-equal result "hello")))

(define-test "string-trim no whitespace"
  (let ([result (string-trim "hello")])
       (assert-equal result "hello")))

(define-test "string-trim only whitespace"
  (let ([result (string-trim "   ")])
       (assert-equal result "")))

(define-test "string-trim tabs and spaces"
  (let ([result (string-trim "\t hello \t")])
       (assert-equal result "hello")))

(define-test "drop-while basic"
  (let ([result (drop-while (lambda (x) (< x 3)) '(1 2 3 4 5))])
       (assert-equal result '(3 4 5))))

(define-test "drop-while empty"
  (let ([result (drop-while (lambda (x) #t) '())])
       (assert-equal result '())))

(define-test "drop-while all match"
  (let ([result (drop-while (lambda (x) #t) '(1 2 3))])
       (assert-equal result '())))

(define-test "drop-while none match"
  (let ([result (drop-while (lambda (x) #f) '(1 2 3))])
       (assert-equal result '(1 2 3))))

;;; ============================================================
;;; OBJ Line Parsing Tests
;;; ============================================================

(define-test "parse-obj-line empty"
  (let ([result (parse-obj-line "")])
       (assert-false result)))

(define-test "parse-obj-line comment"
  (let ([result (parse-obj-line "# this is a comment")])
       (assert-false result)))

(define-test "parse-obj-line vertex"
  (let ([result (parse-obj-line "v 1.0 2.0 3.0")])
       (assert-true (pair? result))
       (assert-equal (car result) 'vertex)
       (let ([v (cdr result)])
            (assert-true (approx= (vec3-x v) 1.0 0.001))
            (assert-true (approx= (vec3-y v) 2.0 0.001))
            (assert-true (approx= (vec3-z v) 3.0 0.001)))))

(define-test "parse-obj-line vertex negative"
  (let ([result (parse-obj-line "v -1.5 -2.5 -3.5")])
       (assert-true (pair? result))
       (let ([v (cdr result)])
            (assert-true (approx= (vec3-x v) -1.5 0.001))
            (assert-true (approx= (vec3-y v) -2.5 0.001))
            (assert-true (approx= (vec3-z v) -3.5 0.001)))))

(define-test "parse-obj-line face simple"
  (let ([result (parse-obj-line "f 1 2 3")])
       (assert-true (pair? result))
       (assert-equal (car result) 'face)
       (let ([indices (cdr result)])
            (assert-equal (length indices) 3)
            (assert-equal (car indices) 1)
            (assert-equal (cadr indices) 2)
            (assert-equal (caddr indices) 3))))

(define-test "parse-obj-line face with texture coords"
  (let ([result (parse-obj-line "f 1/1 2/2 3/3")])
       (assert-true (pair? result))
       (assert-equal (car result) 'face)
       (let ([indices (cdr result)])
            (assert-equal (length indices) 3)
            ;; Should extract vertex indices only
            (assert-equal (car indices) 1))))

(define-test "parse-obj-line face with normals"
  (let ([result (parse-obj-line "f 1/1/1 2/2/2 3/3/3")])
       (assert-true (pair? result))
       (assert-equal (car result) 'face)
       (let ([indices (cdr result)])
            (assert-equal (length indices) 3))))

(define-test "parse-obj-line face no texture"
  (let ([result (parse-obj-line "f 1//1 2//2 3//3")])
       (assert-true (pair? result))
       (assert-equal (car result) 'face)))

(define-test "parse-obj-line quad"
  (let ([result (parse-obj-line "f 1 2 3 4")])
       (assert-true (pair? result))
       (let ([indices (cdr result)])
            (assert-equal (length indices) 4))))

(define-test "parse-obj-line unknown directive"
  (let ([result (parse-obj-line "vn 0 1 0")])
       ;; Normal directive should be ignored
       (assert-false result)))

;;; ============================================================
;;; Vertex Parsing Tests
;;; ============================================================

(define-test "parse-vertex basic"
  (let ([v (parse-vertex '("1.0" "2.0" "3.0"))])
       (assert-true (approx= (vec3-x v) 1.0 0.001))
       (assert-true (approx= (vec3-y v) 2.0 0.001))
       (assert-true (approx= (vec3-z v) 3.0 0.001))))

(define-test "parse-vertex insufficient coords"
  (let ([v (parse-vertex '("1.0" "2.0"))])
       ;; Should return zero for missing z
       (assert-true (vec3? v))))

(define-test "parse-vertex extra coords"
  (let ([v (parse-vertex '("1.0" "2.0" "3.0" "1.0"))])
       ;; Extra coords should be ignored
       (assert-true (approx= (vec3-x v) 1.0 0.001))
       (assert-true (approx= (vec3-y v) 2.0 0.001))
       (assert-true (approx= (vec3-z v) 3.0 0.001))))

;;; ============================================================
;;; Face Parsing Tests
;;; ============================================================

(define-test "parse-face simple indices"
  (let ([indices (parse-face '("1" "2" "3"))])
       (assert-equal indices '(1 2 3))))

(define-test "parse-face with slashes"
  (let ([indices (parse-face '("1/2/3" "4/5/6" "7/8/9"))])
       ;; Should extract only vertex indices
       (assert-equal indices '(1 4 7))))

;;; ============================================================
;;; Vertex Index Resolution Tests
;;; ============================================================

(define-test "get-vertex positive index"
  (let* ([vertices (vector (vec3 1 0 0) (vec3 2 0 0) (vec3 3 0 0))]
         [v (get-vertex vertices 2)])
        ;; OBJ indices are 1-based
        (assert-true (approx= (vec3-x v) 2.0 0.001))))

(define-test "get-vertex negative index"
  (let* ([vertices (vector (vec3 1 0 0) (vec3 2 0 0) (vec3 3 0 0))]
         [v (get-vertex vertices -1)])
        ;; -1 refers to last vertex
        (assert-true (approx= (vec3-x v) 3.0 0.001))))

(define-test "get-vertex first index"
  (let* ([vertices (vector (vec3 1 0 0) (vec3 2 0 0) (vec3 3 0 0))]
         [v (get-vertex vertices 1)])
        (assert-true (approx= (vec3-x v) 1.0 0.001))))

;;; ============================================================
;;; Face to Triangles Tests
;;; ============================================================

(define-test "face->triangles single triangle"
  (let* ([vertices (vector (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
         [face '(1 2 3)]
         [tris (face->triangles vertices face)])
        (assert-equal (length tris) 1)
        (assert-true (triangle3? (car tris)))))

(define-test "face->triangles quad to two triangles"
  (let* ([vertices (vector (vec3 0 0 0) (vec3 1 0 0) (vec3 1 1 0) (vec3 0 1 0))]
         [face '(1 2 3 4)]
         [tris (face->triangles vertices face)])
        ;; Quad should be fan-triangulated into 2 triangles
        (assert-equal (length tris) 2)))

(define-test "face->triangles pentagon to three triangles"
  (let* ([vertices (vector (vec3 0 0 0) (vec3 1 0 0) (vec3 1.5 0.5 0)
                           (vec3 1 1 0) (vec3 0 1 0))]
         [face '(1 2 3 4 5)]
         [tris (face->triangles vertices face)])
        ;; Pentagon should be fan-triangulated into 3 triangles
        (assert-equal (length tris) 3)))

(define-test "face->triangles degenerate face"
  (let* ([vertices (vector (vec3 0 0 0) (vec3 1 0 0))]
         [face '(1 2)]  ; Only 2 vertices - not a valid face
         [tris (face->triangles vertices face)])
        (assert-equal (length tris) 0)))

;;; ============================================================
;;; Full OBJ Loading Tests
;;; ============================================================

(define-test "load-obj-from-string single triangle"
  (let* ([content "v 0 0 0\nv 1 0 0\nv 0 1 0\nf 1 2 3"]
         [tris (load-obj-from-string content)])
        (assert-equal (length tris) 1)
        (assert-true (triangle3? (car tris)))))

(define-test "load-obj-from-string multiple triangles"
  (let* ([content "v 0 0 0\nv 1 0 0\nv 0 1 0\nv 2 0 0\nv 2 1 0\nf 1 2 3\nf 2 4 5"]
         [tris (load-obj-from-string content)])
        (assert-equal (length tris) 2)))

(define-test "load-obj-from-string with comments"
  (let* ([content "# Cube mesh\nv 0 0 0\n# Another comment\nv 1 0 0\nv 0 1 0\nf 1 2 3"]
         [tris (load-obj-from-string content)])
        (assert-equal (length tris) 1)))

(define-test "load-obj-from-string quad"
  (let* ([content "v 0 0 0\nv 1 0 0\nv 1 1 0\nv 0 1 0\nf 1 2 3 4"]
         [tris (load-obj-from-string content)])
        ;; Quad should produce 2 triangles
        (assert-equal (length tris) 2)))

(define-test "load-obj-from-string empty"
  (let* ([content ""]
         [tris (load-obj-from-string content)])
        (assert-equal (length tris) 0)))

(define-test "load-obj-from-string only comments"
  (let* ([content "# Just a comment\n# Another one"]
         [tris (load-obj-from-string content)])
        (assert-equal (length tris) 0)))

(define-test "load-obj-from-string vertices but no faces"
  (let* ([content "v 0 0 0\nv 1 0 0\nv 0 1 0"]
         [tris (load-obj-from-string content)])
        (assert-equal (length tris) 0)))

(define-test "load-obj-from-string preserves geometry"
  (let* ([content "v 1 2 3\nv 4 5 6\nv 7 8 9\nf 1 2 3"]
         [tris (load-obj-from-string content)]
         [tri (car tris)]
         [p1 (triangle3-p1 tri)])
        (assert-true (approx= (vec3-x p1) 1.0 0.001))
        (assert-true (approx= (vec3-y p1) 2.0 0.001))
        (assert-true (approx= (vec3-z p1) 3.0 0.001))))

;;; ============================================================
;;; Cube OBJ Test
;;; ============================================================

(define-test "load-obj-from-string cube"
  (let* ([content
          "# Simple cube
v -1 -1 -1
v  1 -1 -1
v  1  1 -1
v -1  1 -1
v -1 -1  1
v  1 -1  1
v  1  1  1
v -1  1  1
# Front face
f 1 2 3 4
# Back face
f 5 6 7 8
# Top face
f 4 3 7 8
# Bottom face
f 1 2 6 5
# Right face
f 2 3 7 6
# Left face
f 1 4 8 5
"]
         [tris (load-obj-from-string content)])
        ;; 6 faces * 2 triangles per quad = 12 triangles
        (assert-equal (length tris) 12)
        ;; All should be valid triangles
        (for-each (lambda (tri)
                          (assert-true (triangle3? tri)))
                  tris)))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n")
(display "Tests run:    ")
(display *tests-run*)
(newline)
(display "Tests passed: ")
(display *tests-passed*)
(newline)
(display "Tests failed: ")
(display *tests-failed*)
(newline)

(if (> *tests-failed* 0)
    (exit 1)
    (exit 0))
