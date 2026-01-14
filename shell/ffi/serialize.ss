;;; shell/ffi/serialize.ss — Scheme ↔ Rust Data Serialization
;;;
;;; Converts Scheme data structures (Vec3, Triangle3, BVH) to flat
;;; bytevector format for FFI transfer to Rust.
;;;
;;; Format: All values are little-endian IEEE 754 doubles (8 bytes)
;;;
;;; This is Shell code: handles serialization for FFI boundary.

(load "core/base/prelude.ss")
(load "lattice/linalg/vec3.ss")
(load "lattice/geometry/geometry.ss")
(load "lattice/geometry/bvh.ss")

;;; ====
;;; Vec3 Serialization
;;; ====

;;; vec3->bytes : Vec3 → Bytevector (24 bytes)
;;; Serialize Vec3 to 3 little-endian f64 values
(define (vec3->bytes v)
  (let ([bv (make-bytevector 24)])
       (bytevector-ieee-double-native-set! bv 0  (vec3-x v))
       (bytevector-ieee-double-native-set! bv 8  (vec3-y v))
       (bytevector-ieee-double-native-set! bv 16 (vec3-z v))
       bv))

;;; bytes->vec3 : Bytevector × Offset → Vec3
;;; Deserialize Vec3 from bytevector at offset
(define (bytes->vec3 bv offset)
  (vec3 (bytevector-ieee-double-native-ref bv offset)
        (bytevector-ieee-double-native-ref bv (+ offset 8))
        (bytevector-ieee-double-native-ref bv (+ offset 16))))

;;; ====
;;; Triangle Serialization
;;; ====

;;; triangle->bytes : Triangle3 → Bytevector (72 bytes)
;;; Serialize Triangle to 9 f64 values (3 vertices × 3 components)
(define (triangle->bytes tri)
  (let ([bv (make-bytevector 72)]
        [p1 (triangle3-p1 tri)]
        [p2 (triangle3-p2 tri)]
        [p3 (triangle3-p3 tri)])
       ;; P1
       (bytevector-ieee-double-native-set! bv 0  (vec3-x p1))
       (bytevector-ieee-double-native-set! bv 8  (vec3-y p1))
       (bytevector-ieee-double-native-set! bv 16 (vec3-z p1))
       ;; P2
       (bytevector-ieee-double-native-set! bv 24 (vec3-x p2))
       (bytevector-ieee-double-native-set! bv 32 (vec3-y p2))
       (bytevector-ieee-double-native-set! bv 40 (vec3-z p2))
       ;; P3
       (bytevector-ieee-double-native-set! bv 48 (vec3-x p3))
       (bytevector-ieee-double-native-set! bv 56 (vec3-y p3))
       (bytevector-ieee-double-native-set! bv 64 (vec3-z p3))
       bv))

;;; bytes->triangle : Bytevector × Offset → Triangle3
(define (bytes->triangle bv offset)
  (triangle3 (bytes->vec3 bv offset)
             (bytes->vec3 bv (+ offset 24))
             (bytes->vec3 bv (+ offset 48))))

;;; ====
;;; AABB Serialization
;;; ====

;;; aabb->bytes : AABB → Bytevector (48 bytes)
;;; Serialize AABB to 6 f64 values (min + max)
(define (aabb->bytes box)
  (let ([bv (make-bytevector 48)]
        [min-pt (aabb-min box)]
        [max-pt (aabb-max box)])
       ;; Min
       (bytevector-ieee-double-native-set! bv 0  (vec3-x min-pt))
       (bytevector-ieee-double-native-set! bv 8  (vec3-y min-pt))
       (bytevector-ieee-double-native-set! bv 16 (vec3-z min-pt))
       ;; Max
       (bytevector-ieee-double-native-set! bv 24 (vec3-x max-pt))
       (bytevector-ieee-double-native-set! bv 32 (vec3-y max-pt))
       (bytevector-ieee-double-native-set! bv 40 (vec3-z max-pt))
       bv))

;;; bytes->aabb : Bytevector × Offset → AABB
(define (bytes->aabb bv offset)
  (aabb (bytes->vec3 bv offset)
        (bytes->vec3 bv (+ offset 24))))

;;; ====
;;; BVH Serialization
;;; ====

;;; BVH Serialization Format:
;;;
;;; Header (16 bytes):
;;;   [0-3]   u32: magic number (0x42564801 = "BVH\x01")
;;;   [4-7]   u32: number of nodes
;;;   [8-11]  u32: number of triangles
;;;   [12-15] u32: reserved
;;;
;;; Nodes (variable):
;;;   Each node (64 bytes, 8-byte aligned):
;;;     [0]     u8:  node type (0=internal, 1=leaf)
;;;     [1-7]   padding (7 bytes)
;;;     [8-55]  AABB: bounding box (48 bytes)
;;;     if internal:
;;;       [56-59] u32: left child index
;;;       [60-63] u32: right child index
;;;     if leaf:
;;;       [56-59] u32: first triangle index
;;;       [60-63] u32: triangle count
;;;
;;; Triangles (variable):
;;;   Each triangle: 72 bytes (9 × f64)

(define *bvh-magic* #x42564801)  ; "BVH\x01"

;;; count-bvh-nodes : BVH → Nat
;;; Count total nodes in BVH
(define (count-bvh-nodes bvh)
  (cond
   [(not bvh) 0]
   [(bvh-leaf? bvh) 1]
   [(bvh-node? bvh)
    (+ 1
       (count-bvh-nodes (bvh-left bvh))
       (count-bvh-nodes (bvh-right bvh)))]
   [else 0]))

;;; collect-bvh-triangles : BVH → (List Triangle3)
;;; Collect all triangles from BVH leaves
(define (collect-bvh-triangles bvh)
  (cond
   [(not bvh) '()]
   [(bvh-leaf? bvh) (bvh-primitives bvh)]
   [(bvh-node? bvh)
    (append (collect-bvh-triangles (bvh-left bvh))
            (collect-bvh-triangles (bvh-right bvh)))]
   [else '()]))

;;; bvh->bytes : BVH → Bytevector
;;; Serialize entire BVH to bytevector
(define (bvh->bytes bvh)
  (let* ([nodes (count-bvh-nodes bvh)]
         [triangles (collect-bvh-triangles bvh)]
         [num-tris (length triangles)]
         ;; Size calculation: header + nodes + triangles
         ;; Each node: 1 (type) + 7 (pad) + 48 (aabb) + 8 (children/indices) = 64 bytes
         ;; Each triangle: 72 bytes
         [node-size 64]
         [header-size 16]
         [nodes-size (* nodes node-size)]
         [tris-size (* num-tris 72)]
         [total-size (+ header-size nodes-size tris-size)]
         [bv (make-bytevector total-size 0)])
        
        ;; Write header
        (bytevector-u32-native-set! bv 0 *bvh-magic*)
        (bytevector-u32-native-set! bv 4 nodes)
        (bytevector-u32-native-set! bv 8 num-tris)
        (bytevector-u32-native-set! bv 12 0)  ; reserved
        
        ;; Write triangles first (for index assignment)
        (let ([tri-offset (+ header-size (* nodes node-size))])
             (let loop ([tris triangles] [offset tri-offset])
                  (unless (null? tris)
                          (let ([tri-bv (triangle->bytes (car tris))])
                               (bytevector-copy! tri-bv 0 bv offset 72)
                               (loop (cdr tris) (+ offset 72))))))
        
        ;; Build triangle index map and write nodes
        (let ([tri-indices (make-hashtable equal-hash equal?)])
             ;; Populate triangle index map
             (let loop ([tris triangles] [idx 0])
                  (unless (null? tris)
                          (hashtable-set! tri-indices (car tris) idx)
                          (loop (cdr tris) (+ idx 1))))
             
             ;; Write nodes (DFS order with index tracking via box)
             (let ([node-counter-box (box 0)])
                  
                  (letrec ([write-node!
                            (lambda (node offset)
                                    (let ([current-index (unbox node-counter-box)])
                                         (set-box! node-counter-box (+ current-index 1))
                                         (cond
                                          [(bvh-leaf? node)
                                           ;; Leaf node (64 bytes: 1 type + 7 pad + 48 aabb + 8 indices)
                                           (bytevector-u8-set! bv offset 1)  ; type=leaf
                                           ;; Write AABB at offset+8 (8-byte aligned)
                                           (let ([aabb-bv (aabb->bytes (bvh-bbox node))])
                                                (bytevector-copy! aabb-bv 0 bv (+ offset 8) 48))
                                           ;; Write triangle indices at offset+56 (4-byte aligned)
                                           (let* ([prims (bvh-primitives node)]
                                                  [first-idx (if (null? prims) 0 (hashtable-ref tri-indices (car prims) 0))]
                                                  [count (length prims)])
                                                 (bytevector-u32-native-set! bv (+ offset 56) first-idx)
                                                 (bytevector-u32-native-set! bv (+ offset 60) count))
                                           (+ offset node-size)]
                                          
                                          [(bvh-node? node)
                                           ;; Internal node (64 bytes: 1 type + 7 pad + 48 aabb + 8 indices)
                                           (bytevector-u8-set! bv offset 0)  ; type=internal
                                           ;; Write AABB at offset+8 (8-byte aligned)
                                           (let ([aabb-bv (aabb->bytes (bvh-bbox node))])
                                                (bytevector-copy! aabb-bv 0 bv (+ offset 8) 48))
                                           ;; Write children indices at offset+56 (4-byte aligned)
                                           (let* ([left-offset (+ offset node-size)]
                                                  [left-index (+ current-index 1)]
                                                  [next-offset (write-node! (bvh-left node) left-offset)]
                                                  [right-index (unbox node-counter-box)]
                                                  [final-offset (write-node! (bvh-right node) next-offset)])
                                                 (bytevector-u32-native-set! bv (+ offset 56) left-index)
                                                 (bytevector-u32-native-set! bv (+ offset 60) right-index)
                                                 final-offset)]
                                          
                                          [else offset])))])
                          
                          (write-node! bvh header-size))))
        bv))

;;; ====
;;; Tests
;;; ====

;;; test-serialization : → Boolean
(define (test-serialization)
  (display "Serialization Tests\n")
  (display "====\n")
  
  ;; Test Vec3
  (display "1. Vec3 round-trip... ")
  (let* ([v (vec3 1.5 2.5 3.5)]
         [bv (vec3->bytes v)]
         [v2 (bytes->vec3 bv 0)])
        (if (and (= (vec3-x v) (vec3-x v2))
                 (= (vec3-y v) (vec3-y v2))
                 (= (vec3-z v) (vec3-z v2)))
            (display "OK\n")
            (begin (display "FAIL\n") (display v2) (newline))))
  
  ;; Test Triangle
  (display "2. Triangle round-trip... ")
  (let* ([tri (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
         [bv (triangle->bytes tri)]
         [tri2 (bytes->triangle bv 0)]
         [p1 (triangle3-p1 tri2)])
        (if (= (vec3-x p1) 0)
            (display "OK\n")
            (begin (display "FAIL\n") (display tri2) (newline))))
  
  ;; Test AABB
  (display "3. AABB round-trip... ")
  (let* ([box (aabb (vec3 -1 -1 -1) (vec3 1 1 1))]
         [bv (aabb->bytes box)]
         [box2 (bytes->aabb bv 0)])
        (if (and (= (vec3-x (aabb-min box2)) -1)
                 (= (vec3-x (aabb-max box2)) 1))
            (display "OK\n")
            (begin (display "FAIL\n") (display box2) (newline))))
  
  ;; Test BVH
  (display "4. BVH serialization... ")
  (let* ([triangles (list
                     (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))
                     (triangle3 (vec3 1 0 0) (vec3 2 0 0) (vec3 1 1 0))
                     (triangle3 (vec3 2 0 0) (vec3 3 0 0) (vec3 2 1 0)))]
         [bvh (bvh-build triangles 2)]
         [bv (bvh->bytes bvh)]
         [magic (bytevector-u32-native-ref bv 0)]
         [num-nodes (bytevector-u32-native-ref bv 4)]
         [num-tris (bytevector-u32-native-ref bv 8)])
        (display (format "~a nodes, ~a tris, ~a bytes... "
                         num-nodes num-tris (bytevector-length bv)))
        (if (and (= magic *bvh-magic*)
                 (= num-tris 3)
                 (> num-nodes 0))
            (display "OK\n")
            (begin (display "FAIL\n"))))
  
  (display "====\n")
  #t)
