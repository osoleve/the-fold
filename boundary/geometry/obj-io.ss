(load "lattice/geometry/obj-loader.ss")
(load "lattice/geometry/mesh-sdf.ss")

(doc 'module 'obj-io)
(doc 'description "File I/O wrappers for OBJ loading — boundary layer for pure lattice parser")
(doc 'layer 'boundary)

(define (load-obj-file filename)
  (doc 'export #t)
  (doc 'type '(-> String (List Triangle3)))
  (doc 'description "Read OBJ file from disk and parse into triangle list")
  (let ([content (call-with-input-file filename
                                       (lambda (port)
                                               (get-string-all port)))])
       (load-obj-from-string content)))

(define (obj->mesh filename)
  (doc 'export #t)
  (doc 'type '(-> String Mesh))
  (doc 'description "Load OBJ file and create mesh with BVH")
  (make-mesh (load-obj-file filename)))
