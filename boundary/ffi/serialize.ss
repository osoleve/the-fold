(load "core/base/prelude.ss")
(load "lattice/linalg/vec3.ss")
(load "lattice/geometry/geometry.ss")
(load "lattice/geometry/bvh.ss")

(doc 'module 'serialize)
(doc 'description "Scheme ↔ Rust Data Serialization")
(doc 'layer 'boundary)
(doc 'purity 'total)
(doc 'note "Converts Scheme data structures (Vec3, Triangle3, BVH) to flat bytevector format for FFI transfer to Rust.")
(doc 'note "Format: All values are little-endian IEEE 754 doubles (8 bytes)")

(doc 'section 'vec3-serialization)

(define (vec3->bytes v)
  (doc 'type (-> Vec3 Bytevector))
  (doc 'description "Serialize Vec3 to 3 little-endian f64 values (24 bytes)")
  (let ([bv (make-bytevector 24)])
       (bytevector-ieee-double-native-set! bv 0  (vec3-x v))
       (bytevector-ieee-double-native-set! bv 8  (vec3-y v))
       (bytevector-ieee-double-native-set! bv 16 (vec3-z v))
       bv))

(define (bytes->vec3 bv offset)
  (doc 'type (-> Bytevector Nat Vec3))
  (doc 'description "Deserialize Vec3 from bytevector at offset")
  (vec3 (bytevector-ieee-double-native-ref bv offset)
        (bytevector-ieee-double-native-ref bv (+ offset 8))
        (bytevector-ieee-double-native-ref bv (+ offset 16))))

(doc 'section 'triangle-serialization)

(define (triangle->bytes tri)
  (doc 'type (-> Triangle3 Bytevector))
  (doc 'description "Serialize Triangle to 9 f64 values (72 bytes): 3 vertices × 3 components")
  (let ([bv (make-bytevector 72)]
        [p1 (triangle3-p1 tri)]
        [p2 (triangle3-p2 tri)]
        [p3 (triangle3-p3 tri)])
       (bytevector-ieee-double-native-set! bv 0  (vec3-x p1))
       (bytevector-ieee-double-native-set! bv 8  (vec3-y p1))
       (bytevector-ieee-double-native-set! bv 16 (vec3-z p1))
       (bytevector-ieee-double-native-set! bv 24 (vec3-x p2))
       (bytevector-ieee-double-native-set! bv 32 (vec3-y p2))
       (bytevector-ieee-double-native-set! bv 40 (vec3-z p2))
       (bytevector-ieee-double-native-set! bv 48 (vec3-x p3))
       (bytevector-ieee-double-native-set! bv 56 (vec3-y p3))
       (bytevector-ieee-double-native-set! bv 64 (vec3-z p3))
       bv))

(define (bytes->triangle bv offset)
  (doc 'type (-> Bytevector Nat Triangle3))
  (doc 'description "Deserialize Triangle3 from bytevector at offset")
  (triangle3 (bytes->vec3 bv offset)
             (bytes->vec3 bv (+ offset 24))
             (bytes->vec3 bv (+ offset 48))))

(doc 'section 'aabb-serialization)

(define (aabb->bytes box)
  (doc 'type (-> AABB Bytevector))
  (doc 'description "Serialize AABB to 6 f64 values (48 bytes): min + max")
  (let ([bv (make-bytevector 48)]
        [min-pt (aabb-min box)]
        [max-pt (aabb-max box)])
       (bytevector-ieee-double-native-set! bv 0  (vec3-x min-pt))
       (bytevector-ieee-double-native-set! bv 8  (vec3-y min-pt))
       (bytevector-ieee-double-native-set! bv 16 (vec3-z min-pt))
       (bytevector-ieee-double-native-set! bv 24 (vec3-x max-pt))
       (bytevector-ieee-double-native-set! bv 32 (vec3-y max-pt))
       (bytevector-ieee-double-native-set! bv 40 (vec3-z max-pt))
       bv))

(define (bytes->aabb bv offset)
  (doc 'type (-> Bytevector Nat AABB))
  (doc 'description "Deserialize AABB from bytevector at offset")
  (aabb (bytes->vec3 bv offset)
        (bytes->vec3 bv (+ offset 24))))

(doc 'section 'bvh-serialization)

(doc 'note "BVH Serialization Format:")
(doc 'note "Header (16 bytes): [0-3] u32 magic (0x42564801='BVH\\x01'), [4-7] u32 num-nodes, [8-11] u32 num-triangles, [12-15] u32 reserved")
(doc 'note "Nodes (64 bytes each, 8-byte aligned): [0] u8 type (0=internal, 1=leaf), [1-7] padding, [8-55] AABB (48 bytes)")
(doc 'note "If internal: [56-59] u32 left-child-index, [60-63] u32 right-child-index")
(doc 'note "If leaf: [56-59] u32 first-triangle-index, [60-63] u32 triangle-count")
(doc 'note "Triangles (72 bytes each): 9 × f64")

(define *bvh-magic* #x42564801)
  (doc *bvh-magic* 'description "BVH magic number: 'BVH\\x01'")

(define (count-bvh-nodes bvh)
  (doc 'type (-> BVH Nat))
  (doc 'description "Count total nodes in BVH")
  (cond
   [(not bvh) 0]
   [(bvh-leaf? bvh) 1]
   [(bvh-node? bvh)
    (+ 1
       (count-bvh-nodes (bvh-left bvh))
       (count-bvh-nodes (bvh-right bvh)))]
   [else 0]))

(define (collect-bvh-triangles bvh)
  (doc 'type (-> BVH (List Triangle3)))
  (doc 'description "Collect all triangles from BVH leaves")
  (cond
   [(not bvh) '()]
   [(bvh-leaf? bvh) (bvh-primitives bvh)]
   [(bvh-node? bvh)
    (append (collect-bvh-triangles (bvh-left bvh))
            (collect-bvh-triangles (bvh-right bvh)))]
   [else '()]))

(define (bvh->bytes bvh)
  (doc 'type (-> BVH Bytevector))
  (doc 'description "Serialize entire BVH to bytevector")
  (let* ([nodes (count-bvh-nodes bvh)]
         [triangles (collect-bvh-triangles bvh)]
         [num-tris (length triangles)]
         [node-size 64]
         [header-size 16]
         [nodes-size (* nodes node-size)]
         [tris-size (* num-tris 72)]
         [total-size (+ header-size nodes-size tris-size)]
         [bv (make-bytevector total-size 0)])
        (bytevector-u32-native-set! bv 0 *bvh-magic*)
        (bytevector-u32-native-set! bv 4 nodes)
        (bytevector-u32-native-set! bv 8 num-tris)
        (bytevector-u32-native-set! bv 12 0)
        (let ([tri-offset (+ header-size (* nodes node-size))])
             (let loop ([tris triangles] [offset tri-offset])
                  (unless (null? tris)
                          (let ([tri-bv (triangle->bytes (car tris))])
                               (bytevector-copy! tri-bv 0 bv offset 72)
                               (loop (cdr tris) (+ offset 72))))))
        (let ([tri-indices (make-hashtable equal-hash equal?)])
             (let loop ([tris triangles] [idx 0])
                  (unless (null? tris)
                          (hashtable-set! tri-indices (car tris) idx)
                          (loop (cdr tris) (+ idx 1))))
             (let ([node-counter-box (box 0)])
                  (letrec ([write-node!
                            (lambda (node offset)
                                    (let ([current-index (unbox node-counter-box)])
                                         (set-box! node-counter-box (+ current-index 1))
                                         (cond
                                          [(bvh-leaf? node)
                                           (bytevector-u8-set! bv offset 1)
                                           (let ([aabb-bv (aabb->bytes (bvh-bbox node))])
                                                (bytevector-copy! aabb-bv 0 bv (+ offset 8) 48))
                                           (let* ([prims (bvh-primitives node)]
                                                  [first-idx (if (null? prims) 0 (hashtable-ref tri-indices (car prims) 0))]
                                                  [count (length prims)])
                                                 (bytevector-u32-native-set! bv (+ offset 56) first-idx)
                                                 (bytevector-u32-native-set! bv (+ offset 60) count))
                                           (+ offset node-size)]
                                          [(bvh-node? node)
                                           (bytevector-u8-set! bv offset 0)
                                           (let ([aabb-bv (aabb->bytes (bvh-bbox node))])
                                                (bytevector-copy! aabb-bv 0 bv (+ offset 8) 48))
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

(doc 'section 'tests)

(define (test-serialization)
  (doc 'type (-> Boolean))
  (doc 'description "Run serialization tests")
  (display "Serialization Tests\n")
  (display "====\n")
  (display "1. Vec3 round-trip... ")
  (let* ([v (vec3 1.5 2.5 3.5)]
         [bv (vec3->bytes v)]
         [v2 (bytes->vec3 bv 0)])
        (if (and (= (vec3-x v) (vec3-x v2))
                 (= (vec3-y v) (vec3-y v2))
                 (= (vec3-z v) (vec3-z v2)))
            (display "OK\n")
            (begin (display "FAIL\n") (display v2) (newline))))
  (display "2. Triangle round-trip... ")
  (let* ([tri (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0))]
         [bv (triangle->bytes tri)]
         [tri2 (bytes->triangle bv 0)]
         [p1 (triangle3-p1 tri2)])
        (if (= (vec3-x p1) 0)
            (display "OK\n")
            (begin (display "FAIL\n") (display tri2) (newline))))
  (display "3. AABB round-trip... ")
  (let* ([box (aabb (vec3 -1 -1 -1) (vec3 1 1 1))]
         [bv (aabb->bytes box)]
         [box2 (bytes->aabb bv 0)])
        (if (and (= (vec3-x (aabb-min box2)) -1)
                 (= (vec3-x (aabb-max box2)) 1))
            (display "OK\n")
            (begin (display "FAIL\n") (display box2) (newline))))
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
