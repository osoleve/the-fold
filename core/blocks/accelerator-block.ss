;;; core/blocks/accelerator-block.ss — Block Format for Rust Accelerator Metadata
;;;
;;; Defines the block structure for storing accelerator manifests in CAS.
;;; Each accelerator is stored as a block containing:
;;;   - Tag: 'accelerator
;;;   - Payload: S-expression manifest (UTF-8 encoded)
;;;   - Refs: [source-hash, target-hash]
;;;
;;; This is Core code: defines the format, pure operations.

(load "core/base/prelude.ss")
(load "core/blocks/block.ss")

;;; ============================================================
;;; Accelerator Manifest Format
;;; ============================================================

;;; Example manifest:
;;; (accelerator
;;;   (name "bvh-closest-point")
;;;   (version "0.1.0")
;;;   (target "lattice/geometry/bvh.ss:bvh-closest-point")
;;;   (rust-fn "fold_bvh_closest_point")
;;;   (signature
;;;     (inputs ((bvh . "BVH") (point . "Vec3") (fuel . "Nat")))
;;;     (outputs ((status . "Enum") (closest . "Vec3?") (distance . "Real?") (fuel . "Nat"))))
;;;   (fuel-model
;;;     (base 5) (per-node 2) (per-aabb 3) (per-triangle 10))
;;;   (source-hash "00abc123...")
;;;   (scheme-hash "00def456..."))

;;; ============================================================
;;; Manifest Construction
;;; ============================================================

;;; make-accelerator-manifest : String × String × String × String × List × List × List × Bytevector × Bytevector → List
;;; Create an accelerator manifest S-expression
(define (make-accelerator-manifest name version target rust-fn
                                   inputs outputs fuel-model
                                   source-hash scheme-hash)
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

;;; ============================================================
;;; Block Operations
;;; ============================================================

;;; make-accelerator-block : List × Bytevector × Bytevector → Block
;;; Create a block containing the accelerator manifest
;;; source-hash: hash of block containing Rust source code
;;; scheme-hash: hash of block containing original Scheme code
(define (make-accelerator-block manifest source-hash scheme-hash)
  (let ([payload (string->utf8 (format "~s" manifest))])
       (make-block 'accelerator payload (vector source-hash scheme-hash))))

;;; accelerator-block? : Block → Boolean
;;; Check if block is an accelerator block
(define (accelerator-block? blk)
  (and (block? blk)
       (eq? (block-tag blk) 'accelerator)))

;;; accelerator-block-manifest : Block → List
;;; Extract manifest from accelerator block
(define (accelerator-block-manifest blk)
  (let ([str (utf8->string (block-payload blk))])
       (read (open-input-string str))))

;;; accelerator-block-source-hash : Block → Bytevector
;;; Get the hash of the Rust source block
(define (accelerator-block-source-hash blk)
  (vector-ref (block-refs blk) 0))

;;; accelerator-block-scheme-hash : Block → Bytevector
;;; Get the hash of the original Scheme code block
(define (accelerator-block-scheme-hash blk)
  (vector-ref (block-refs blk) 1))

;;; ============================================================
;;; Manifest Accessors
;;; ============================================================

;;; manifest-get : List × Symbol → (+ Any #f)
;;; Get a field from manifest
(define (manifest-get manifest key)
  (let ([entry (assq key (cdr manifest))])
       (and entry (cadr entry))))

;;; accelerator-name : List → String
(define (accelerator-name manifest)
  (manifest-get manifest 'name))

;;; accelerator-version : List → String
(define (accelerator-version manifest)
  (manifest-get manifest 'version))

;;; accelerator-target : List → String
(define (accelerator-target manifest)
  (manifest-get manifest 'target))

;;; accelerator-rust-fn : List → String
(define (accelerator-rust-fn manifest)
  (manifest-get manifest 'rust-fn))

;;; accelerator-fuel-model : List → List
(define (accelerator-fuel-model manifest)
  (let ([fm (assq 'fuel-model (cdr manifest))])
       (and fm (cdr fm))))

;;; ============================================================
;;; Rust Source Block
;;; ============================================================

;;; make-rust-source-block : String → Block
;;; Create a block containing Rust source code
(define (make-rust-source-block rust-source)
  (make-block 'rust-source
              (string->utf8 rust-source)
              '#()))

;;; rust-source-block? : Block → Boolean
(define (rust-source-block? blk)
  (and (block? blk)
       (eq? (block-tag blk) 'rust-source)))

;;; rust-source-block-code : Block → String
(define (rust-source-block-code blk)
  (utf8->string (block-payload blk)))

;;; ============================================================
;;; Example: BVH Accelerator Manifest
;;; ============================================================

;;; bvh-closest-point-manifest : Bytevector × Bytevector → List
;;; Create manifest for the BVH closest-point accelerator
(define (bvh-closest-point-manifest source-hash scheme-hash)
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

;;; bvh-intersect-ray-manifest : Bytevector × Bytevector → List
;;; Create manifest for the BVH ray intersection accelerator
(define (bvh-intersect-ray-manifest source-hash scheme-hash)
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

;;; ============================================================
;;; Tests
;;; ============================================================

(define (test-accelerator-block)
  (display "Accelerator Block Tests\n")
  (display "=======================\n")
  
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
  
  (display "=======================\n")
  #t)
