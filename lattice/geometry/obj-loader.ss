(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'geometry)

(doc 'module 'obj-loader)
(doc 'description "Wavefront OBJ file loader for triangle meshes")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'provides "Load triangle meshes from .obj files; supports vertices (v), faces (f), basic format")

(doc 'section 'obj-parsing)

(define (parse-obj-line line)
  (doc 'export #t)
  (doc 'type '(-> String (Maybe (Pair Symbol Data))))
  (doc 'description "Parse a single line of OBJ file")
  (let ([trimmed (string-trim line)])
       (cond
        [(string=? trimmed "") #f]
        [(char=? (string-ref trimmed 0) #\#) #f]  ; Comment
        [else
         (let ([parts (string-split trimmed #\space)])
              (cond
               [(null? parts) #f]
               [(string=? (car parts) "v")
                (cons 'vertex (parse-vertex (cdr parts)))]
               [(string=? (car parts) "f")
                (cons 'face (parse-face (cdr parts)))]
               [else #f]))])))  ; Ignore other directives

(define (parse-vertex parts)
  (doc 'export #t)
  (doc 'type '(-> (List String) Vec3))
  (let ([nums (map string->number (filter (lambda (s) (> (string-length s) 0)) parts))])
       (if (>= (length nums) 3)
           (vec3 (car nums) (cadr nums) (caddr nums))
           (vec3 0 0 0))))

(define (parse-face parts)
  (doc 'export #t)
  (doc 'type '(-> (List String) (List Number)))
  (doc 'description "Handles v, v/vt, v/vt/vn, v//vn formats")
  (map (lambda (part)
               (let ([idx-str (car (string-split part #\/))])
                    (string->number idx-str)))
       (filter (lambda (s) (> (string-length s) 0)) parts)))

;; string-split and string-trim are provided by prelude

(doc 'section 'obj-loading)

(define (load-obj-from-string content)
  (doc 'export #t)
  (doc 'type '(-> String (List Triangle3)))
  (doc 'description "Parse OBJ content and return list of triangles")
  (let* ([lines (string-split content #\newline)]
         [parsed (filter identity (map parse-obj-line lines))]
         [vertices (list->vector
                    (map cdr (filter (lambda (p) (eq? (car p) 'vertex)) parsed)))]
         [faces (map cdr (filter (lambda (p) (eq? (car p) 'face)) parsed))])
        (faces->triangles vertices faces)))

(define (faces->triangles vertices faces)
  (doc 'export #t)
  (doc 'type '(-> (Vector Vec3) (List (List Number)) (List Triangle3)))
  (doc 'description "Convert face indices to triangles (handles n-gons by fan triangulation)")
  (append-map (lambda (face)
               (face->triangles vertices face))
             faces))

(define (face->triangles vertices face)
  (doc 'export #t)
  (doc 'type '(-> (Vector Vec3) (List Number) (List Triangle3)))
  (doc 'description "Fan triangulation: for face [v0, v1, v2, v3, ...] creates triangles (v0, v1, v2), (v0, v2, v3), etc.")
  (if (< (length face) 3)
      '()
      (let ([v0 (get-vertex vertices (car face))])
           (let loop ([rest (cddr face)]
                      [prev (get-vertex vertices (cadr face))]
                      [tris '()])
                (if (null? rest)
                    (reverse tris)
                    (let ([curr (get-vertex vertices (car rest))])
                         (loop (cdr rest)
                               curr
                               (cons (triangle3 v0 prev curr) tris))))))))

(define (get-vertex vertices idx)
  (doc 'export #t)
  (doc 'type '(-> (Vector Vec3) Number Vec3))
  (doc 'description "OBJ indices are 1-based, can be negative (relative)")
  (let ([n (vector-length vertices)])
       (if (< idx 0)
           (vector-ref vertices (+ n idx))
           (vector-ref vertices (- idx 1)))))

