;;; core/geometry/obj-loader.ss — Wavefront OBJ File Loader
;;;
;;; Loads triangle meshes from .obj files.
;;; Supports: vertices (v), faces (f), basic format
;;;
;;; This is Core code: pure, total, assumes well-formed input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - geometry.ss

(load "core/base/prelude.ss")
(load "core/geometry/geometry.ss")

;;; ============================================================
;;; OBJ Parsing
;;; ============================================================

;;; parse-obj-line : String → (Symbol . Data) | #f
;;; Parse a single line of OBJ file
(define (parse-obj-line line)
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

;;; parse-vertex : (List String) → Vec3
(define (parse-vertex parts)
  (let ([nums (map string->number (filter (lambda (s) (> (string-length s) 0)) parts))])
       (if (>= (length nums) 3)
           (vec3 (car nums) (cadr nums) (caddr nums))
           (vec3 0 0 0))))

;;; parse-face : (List String) → (List Number)
;;; Handles "v", "v/vt", "v/vt/vn", "v//vn" formats
(define (parse-face parts)
  (map (lambda (part)
               (let ([idx-str (car (string-split part #\/))])
                    (string->number idx-str)))
       (filter (lambda (s) (> (string-length s) 0)) parts)))

;;; string-split : String × Char → (List String)
(define (string-split str delim)
  (let loop ([chars (string->list str)]
             [current '()]
             [result '()])
       (cond
        [(null? chars)
         (reverse (if (null? current)
                      result
                      (cons (list->string (reverse current)) result)))]
        [(char=? (car chars) delim)
         (loop (cdr chars)
               '()
               (if (null? current)
                   result
                   (cons (list->string (reverse current)) result)))]
        [else
         (loop (cdr chars)
               (cons (car chars) current)
               result)])))

;;; string-trim : String → String
(define (string-trim str)
  (let* ([chars (string->list str)]
         [trimmed (drop-while char-whitespace?
                              (reverse (drop-while char-whitespace? chars)))])
        (list->string (reverse trimmed))))

;;; drop-while : (α → Bool) × (List α) → (List α)
(define (drop-while pred lst)
  (cond
   [(null? lst) '()]
   [(pred (car lst)) (drop-while pred (cdr lst))]
   [else lst]))

;;; ============================================================
;;; OBJ Loading
;;; ============================================================

;;; load-obj-from-string : String → (List Triangle3)
;;; Parse OBJ content and return list of triangles
(define (load-obj-from-string content)
  (let* ([lines (string-split content #\newline)]
         [parsed (filter identity (map parse-obj-line lines))]
         [vertices (list->vector
                    (map cdr (filter (lambda (p) (eq? (car p) 'vertex)) parsed)))]
         [faces (map cdr (filter (lambda (p) (eq? (car p) 'face)) parsed))])
        (faces->triangles vertices faces)))

;;; faces->triangles : (Vector Vec3) × (List (List Number)) → (List Triangle3)
;;; Convert face indices to triangles (handles n-gons by fan triangulation)
(define (faces->triangles vertices faces)
  (apply append
         (map (lambda (face)
                      (face->triangles vertices face))
              faces)))

;;; face->triangles : (Vector Vec3) × (List Number) → (List Triangle3)
;;; Fan triangulation: for face [v0, v1, v2, v3, ...] creates triangles
;;; (v0, v1, v2), (v0, v2, v3), etc.
(define (face->triangles vertices face)
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

;;; get-vertex : (Vector Vec3) × Number → Vec3
;;; OBJ indices are 1-based, can be negative (relative)
(define (get-vertex vertices idx)
  (let ([n (vector-length vertices)])
       (if (< idx 0)
           (vector-ref vertices (+ n idx))
           (vector-ref vertices (- idx 1)))))

;;; ============================================================
;;; File Loading
;;; ============================================================

;;; load-obj-file : String → (List Triangle3)
(define (load-obj-file filename)
  (let ([content (call-with-input-file filename
                                       (lambda (port)
                                               (get-string-all port)))])
       (load-obj-from-string content)))

;;; obj->mesh : String → Mesh
;;; Load OBJ file and create mesh with BVH
(define (obj->mesh filename)
  (load "core/geometry/mesh-sdf.ss")
  (make-mesh (load-obj-file filename)))
