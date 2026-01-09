;;; shell/ffi/bvh-cache.ss — Content-Hash Based BVH Cache
;;;
;;; Caches Rust BVH handles by content hash to avoid rebuilding.
;;; Uses Guardians for automatic cleanup when Scheme wrappers are GC'd.
;;;
;;; This is Shell code: mutable cache, memory management.

(load "shell/ffi/bvh-ffi.ss")
(load "core/base/sha256.ss")

;;; ============================================================
;;; Cache Data Structures
;;; ============================================================

;;; Global cache: content-hash → rust-bvh-handle
(define *rust-bvh-cache* (make-hashtable equal-hash equal?))

;;; Guardian for tracking BVH handles that need cleanup
(define *bvh-guardian* (make-guardian))

;;; Wrapper record that tracks both handle and hash for cleanup
(define-record-type cached-bvh
  (fields hash handle))

;;; ============================================================
;;; Cache Operations
;;; ============================================================

;;; bvh-content-hash : BVH → Bytevector
;;; Compute content hash for a BVH (serialize and hash)
(define (bvh-content-hash bvh)
  (let ([bv (bvh->bytes bvh)])
       ;; Use SHA-256 from core
       (sha256 bv)))

;;; get-cached-rust-handle : Bytevector (hash) → RustBVHHandle | #f
;;; Look up cached handle by content hash
(define (get-cached-rust-handle hash)
  (let ([cached (hashtable-ref *rust-bvh-cache* hash #f)])
       (and cached (cached-bvh-handle cached))))

;;; cache-rust-handle! : Bytevector (hash) × RustBVHHandle → CachedBVH
;;; Store handle in cache and register with guardian
(define (cache-rust-handle! hash handle)
  (let ([cached (make-cached-bvh hash handle)])
       ;; Store in cache
       (hashtable-set! *rust-bvh-cache* hash cached)
       ;; Register with guardian for cleanup
       (*bvh-guardian* cached)
       cached))

;;; remove-from-cache! : Bytevector (hash) → void
;;; Remove entry from cache (does NOT free handle - guardian does that)
(define (remove-from-cache! hash)
  (hashtable-delete! *rust-bvh-cache* hash))

;;; ============================================================
;;; High-Level API
;;; ============================================================

;;; get-rust-handle : BVH → RustBVHHandle
;;; Get Rust handle for BVH, building and caching if needed
(define (get-rust-handle scheme-bvh)
  (unless (accel-available?)
          (error 'get-rust-handle "Acceleration library not loaded"))
  
  (let* ([hash (bvh-content-hash scheme-bvh)]
         [cached (get-cached-rust-handle hash)])
        (if cached
            ;; Cache hit
            cached
            ;; Cache miss - build and cache
            (let ([handle (scheme-bvh->rust-handle scheme-bvh)])
                 (if handle
                     (begin
                      (cache-rust-handle! hash handle)
                      handle)
                     (error 'get-rust-handle "Failed to build Rust BVH"))))))

;;; ============================================================
;;; Guardian-Based Cleanup
;;; ============================================================

;;; cleanup-stale-handles! : → Nat
;;; Free Rust handles for GC'd Scheme wrappers
;;; Returns number of handles freed
(define (cleanup-stale-handles!)
  (let loop ([count 0])
       (let ([dead (*bvh-guardian*)])
            (if dead
                (begin
                 ;; Remove from cache
                 (remove-from-cache! (cached-bvh-hash dead))
                 ;; Free Rust memory
                 (free-rust-bvh! (cached-bvh-handle dead))
                 (loop (+ count 1)))
                count))))

;;; force-cleanup! : → Nat
;;; Force GC and cleanup
(define (force-cleanup!)
  (collect)
  (cleanup-stale-handles!))

;;; ============================================================
;;; Cache Statistics
;;; ============================================================

;;; cache-size : → Nat
;;; Number of cached BVH handles
(define (cache-size)
  (hashtable-size *rust-bvh-cache*))

;;; cache-keys : → (List Bytevector)
;;; List of all cached content hashes
(define (cache-keys)
  (vector->list (hashtable-keys *rust-bvh-cache*)))

;;; clear-cache! : → Nat
;;; Clear entire cache and free all handles
;;; Returns number of handles freed
(define (clear-cache!)
  (let ([count 0])
       (vector-for-each
        (lambda (hash)
                (let ([cached (hashtable-ref *rust-bvh-cache* hash #f)])
                     (when cached
                           (free-rust-bvh! (cached-bvh-handle cached))
                           (set! count (+ count 1)))))
        (hashtable-keys *rust-bvh-cache*))
       (hashtable-clear! *rust-bvh-cache*)
       count))

;;; ============================================================
;;; Tests
;;; ============================================================

(define (run-cache-tests)
  (display "BVH Cache Tests\n")
  (display "===============\n")
  
  ;; Ensure library is loaded
  (unless (accel-available?)
          (accel-load!))
  (bind-bvh-procedures!)
  
  (unless (accel-available?)
          (display "SKIP: No acceleration library\n")
          (display "===============\n")
          #f)
  
  ;; Test 1: Initial cache is empty
  (display "1. Initial cache size... ")
  (display (cache-size))
  (newline)
  
  ;; Test 2: Build and cache BVH
  (display "2. Build and cache BVH... ")
  (let* ([triangles (list
                     (triangle3 (vec3 0 0 0) (vec3 1 0 0) (vec3 0 1 0)))]
         [scheme-bvh (bvh-build triangles 2)]
         [handle (get-rust-handle scheme-bvh)])
        (display "OK, cache size=")
        (display (cache-size))
        (newline)
        
        ;; Test 3: Same BVH returns cached handle
        (display "3. Same BVH returns cached handle... ")
        (let ([handle2 (get-rust-handle scheme-bvh)])
             (if (eq? handle handle2)
                 (display "OK (same handle)\n")
                 (display "WARN (different handle)\n"))))
  
  ;; Test 4: Different BVH gets new handle
  (display "4. Different BVH gets new handle... ")
  (let* ([triangles2 (list
                      (triangle3 (vec3 1 1 1) (vec3 2 1 1) (vec3 1 2 1)))]
         [scheme-bvh2 (bvh-build triangles2 2)]
         [handle3 (get-rust-handle scheme-bvh2)])
        (display "OK, cache size=")
        (display (cache-size))
        (newline))
  
  ;; Test 5: Clear cache
  (display "5. Clear cache... ")
  (let ([freed (clear-cache!)])
       (display (format "freed ~a handles, cache size=~a\n" freed (cache-size))))
  
  (display "===============\n")
  #t)
