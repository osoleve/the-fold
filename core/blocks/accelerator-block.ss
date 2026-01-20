(load "core/base/prelude.ss")
(load "core/blocks/block.ss")

(doc 'module 'accelerator-block)
(doc 'description "Block format for Rust accelerator metadata. Stores accelerator manifests in CAS.")
(doc 'layer 'core)

(doc 'section 'manifest-format)

(doc 'section 'manifest-construction)

(define (make-accelerator-manifest name version target rust-fn
                                   inputs outputs fuel-model
                                   source-hash scheme-hash)
  (doc 'type (-> String String String String List List List Bytevector Bytevector List))
  (doc 'description "Create an accelerator manifest S-expression.")
  (doc 'export #t)
  `(accelerator
    (name ,name)
    (version ,version)
    (target ,target)
    (rust-fn ,rust-fn)
    (signature
     (inputs ,inputs)
     (outputs ,outputs))
    (fuel-model ,@fuel-model)
    (source-hash ,source-hash)
    (scheme-hash ,scheme-hash)))

(doc 'section 'block-operations)

(define (make-accelerator-block manifest source-hash scheme-hash)
  (doc 'type (-> List Bytevector Bytevector Block))
  (doc 'description "Create block containing accelerator manifest. Refs: [source-hash, scheme-hash].")
  (doc 'export #t)
  (let ([payload (string->utf8 (format "~s" manifest))])
       (make-block 'accelerator payload (vector source-hash scheme-hash))))

(define (accelerator-block? blk)
  (doc 'type (-> Block Boolean))
  (doc 'description "Check if block is an accelerator block.")
  (doc 'export #t)
  (and (block? blk)
       (eq? (block-tag blk) 'accelerator)))

(define (accelerator-block-manifest blk)
  (doc 'type (-> Block List))
  (doc 'description "Extract manifest from accelerator block.")
  (doc 'export #t)
  (let ([str (utf8->string (block-payload blk))])
       (read (open-input-string str))))

(define (accelerator-block-source-hash blk)
  (doc 'type (-> Block Bytevector))
  (doc 'description "Get the hash of the Rust source block.")
  (doc 'export #t)
  (vector-ref (block-refs blk) 0))

(define (accelerator-block-scheme-hash blk)
  (doc 'type (-> Block Bytevector))
  (doc 'description "Get the hash of the original Scheme code block.")
  (doc 'export #t)
  (vector-ref (block-refs blk) 1))

(doc 'section 'manifest-accessors)

(define (manifest-get manifest key)
  (doc 'type (-> List Symbol (Maybe Any)))
  (doc 'description "Get a field from manifest.")
  (doc 'export #t)
  (let ([entry (assq key (cdr manifest))])
       (and entry (cadr entry))))

(define (accelerator-name manifest)
  (doc 'type (-> List String))
  (doc 'description "Extract accelerator name from manifest.")
  (doc 'export #t)
  (manifest-get manifest 'name))

(define (accelerator-version manifest)
  (doc 'type (-> List String))
  (doc 'description "Extract accelerator version from manifest.")
  (doc 'export #t)
  (manifest-get manifest 'version))

(define (accelerator-target manifest)
  (doc 'type (-> List String))
  (doc 'description "Extract accelerator target from manifest.")
  (doc 'export #t)
  (manifest-get manifest 'target))

(define (accelerator-rust-fn manifest)
  (doc 'type (-> List String))
  (doc 'description "Extract Rust function name from manifest.")
  (doc 'export #t)
  (manifest-get manifest 'rust-fn))

(define (accelerator-fuel-model manifest)
  (doc 'type (-> List List))
  (doc 'description "Extract fuel model from manifest.")
  (doc 'export #t)
  (let ([fm (assq 'fuel-model (cdr manifest))])
       (and fm (cdr fm))))

(doc 'section 'rust-source-block)

(define (make-rust-source-block rust-source)
  (doc 'type (-> String Block))
  (doc 'description "Create a block containing Rust source code.")
  (doc 'export #t)
  (make-block 'rust-source
              (string->utf8 rust-source)
              '#()))

(define (rust-source-block? blk)
  (doc 'type (-> Block Boolean))
  (doc 'description "Check if block is a Rust source block.")
  (doc 'export #t)
  (and (block? blk)
       (eq? (block-tag blk) 'rust-source)))

(define (rust-source-block-code blk)
  (doc 'type (-> Block String))
  (doc 'description "Extract Rust source code from block.")
  (doc 'export #t)
  (utf8->string (block-payload blk)))

(doc 'section 'examples)

(define (bvh-closest-point-manifest source-hash scheme-hash)
  (doc 'type (-> Bytevector Bytevector List))
  (doc 'description "Create manifest for the BVH closest-point accelerator.")
  (doc 'export #t)
  (make-accelerator-manifest
   "bvh-closest-point"
   "0.1.0"
   "lattice/geometry/bvh.ss:bvh-closest-point"
   "fold_bvh_closest_point"
   '((bvh . "BVH") (point . "Vec3") (fuel . "Nat"))
   '((status . "Enum") (closest . "Vec3?") (distance . "Real?") (fuel . "Nat"))
   '((base 5) (per-node 2) (per-aabb 3) (per-triangle 10))
   source-hash
   scheme-hash))

(define (bvh-intersect-ray-manifest source-hash scheme-hash)
  (doc 'type (-> Bytevector Bytevector List))
  (doc 'description "Create manifest for the BVH ray intersection accelerator.")
  (doc 'export #t)
  (make-accelerator-manifest
   "bvh-intersect-ray"
   "0.1.0"
   "lattice/geometry/bvh.ss:bvh-intersect-ray"
   "fold_bvh_intersect_ray"
   '((bvh . "BVH") (origin . "Vec3") (direction . "Vec3") (fuel . "Nat"))
   '((status . "Enum") (t . "Real?") (normal . "Vec3?") (fuel . "Nat"))
   '((base 5) (per-node 2) (per-aabb 3) (per-triangle 8))
   source-hash
   scheme-hash))

(doc 'section 'tests)

(define (test-accelerator-block)
  (doc 'type (-> Boolean))
  (doc 'description "Test suite for accelerator block operations.")
  (doc 'export #t)
  (display "Accelerator Block Tests\n")
  (display "====\n")
  
  ;; Test 1: Create manifest
  (display "1. Create manifest... ")
  (let* ([manifest (bvh-closest-point-manifest
                    (string->utf8 "source-hash-placeholder")
                    (string->utf8 "scheme-hash-placeholder"))])
        (display "OK\n")
        (display "   Name: ")
        (display (accelerator-name manifest))
        (newline)
        (display "   Target: ")
        (display (accelerator-target manifest))
        (newline)
        (display "   Rust fn: ")
        (display (accelerator-rust-fn manifest))
        (newline))
  
  ;; Test 2: Create and read block
  (display "2. Create accelerator block... ")
  (let* ([manifest (bvh-closest-point-manifest #vu8(1 2 3) #vu8(4 5 6))]
         [blk (make-accelerator-block manifest #vu8(1 2 3) #vu8(4 5 6))])
        (if (accelerator-block? blk)
            (begin
             (display "OK\n")
             (display "   Tag: ")
             (display (block-tag blk))
             (newline))
            (display "FAIL\n")))
  
  ;; Test 3: Extract manifest from block
  (display "3. Extract manifest from block... ")
  (let* ([manifest (bvh-closest-point-manifest #vu8(1 2 3) #vu8(4 5 6))]
         [blk (make-accelerator-block manifest #vu8(1 2 3) #vu8(4 5 6))]
         [extracted (accelerator-block-manifest blk)])
        (if (equal? (accelerator-name extracted) "bvh-closest-point")
            (display "OK\n")
            (display "FAIL\n")))
  
  ;; Test 4: Create Rust source block
  (display "4. Create Rust source block... ")
  (let* ([source "fn foo() { }"]
         [blk (make-rust-source-block source)]
         [extracted (rust-source-block-code blk)])
        (if (equal? extracted source)
            (display "OK\n")
            (display "FAIL\n")))
  
  (display "====\n")
  #t)
