;;; building-chains.ss --- Linked Structures via Content References
;;;
;;; Build chains, trees, and graphs using content-addressed references.
;;;
;;; Run with: scheme --script docs/examples/content-addressing/building-chains.ss

(load "core/blocks/block.ss")
(load "core/blocks/cas.ss")

;;; ============================================================
;;; A Simple Chain (Linked List of Blocks)
;;; ============================================================

(display "=== Building a Chain ===\n")

(display "
A chain is a sequence where each block points to the previous one.
Like a blockchain or git commit history.

  [head] -> [block 2] -> [block 1] -> [genesis]
")

;;; Genesis block (no predecessor)
(define genesis (make-block 'chain-link "Genesis" '()))
(cas-store! genesis)
(format #t "Genesis: ~a...~%" (substring (block-hash genesis) 0 16))

;;; Add blocks to the chain
(define (add-to-chain prev-hash message)
  (let ([block (make-block 'chain-link message (list prev-hash))])
    (cas-store! block)
    (block-hash block)))

(define link1 (add-to-chain (block-hash genesis) "First entry"))
(define link2 (add-to-chain link1 "Second entry"))
(define link3 (add-to-chain link2 "Third entry"))
(define head link3)

(format #t "Head: ~a...~%" (substring head 0 16))

;;; ============================================================
;;; Walking the Chain
;;; ============================================================

(display "\n=== Walking the Chain ===\n")

(define (walk-chain hash)
  (let ([block (cas-fetch hash)])
    (when block
      (format #t "~a: ~a~%"
              (substring hash 0 12)
              (block-payload block))
      (unless (null? (block-refs block))
        (walk-chain (car (block-refs block)))))))

(walk-chain head)

;;; ============================================================
;;; Branching (Multiple Children)
;;; ============================================================

(display "\n=== Branching ===\n")

(display "
Chains can branch - multiple blocks can reference the same parent.
This is how git branches work.

  [branch-a]  [branch-b]
         \\    /
        [common]
           |
        [root]
")

(define root (make-block 'commit "Root commit" '()))
(cas-store! root)

(define common (make-block 'commit "Common work"
                           (list (block-hash root))))
(cas-store! common)

(define branch-a (make-block 'commit "Feature A"
                             (list (block-hash common))))
(cas-store! branch-a)

(define branch-b (make-block 'commit "Feature B"
                             (list (block-hash common))))
(cas-store! branch-b)

(format #t "Root:     ~a...~%" (substring (block-hash root) 0 12))
(format #t "Common:   ~a...~%" (substring (block-hash common) 0 12))
(format #t "Branch A: ~a...~%" (substring (block-hash branch-a) 0 12))
(format #t "Branch B: ~a...~%" (substring (block-hash branch-b) 0 12))

;;; ============================================================
;;; Merging (Multiple Parents)
;;; ============================================================

(display "\n=== Merging ===\n")

(display "
A merge commit references multiple parents.
")

(define merge (make-block 'commit "Merge A and B"
                          (list (block-hash branch-a)
                                (block-hash branch-b))))
(cas-store! merge)

(format #t "Merge: ~a...~%" (substring (block-hash merge) 0 12))
(format #t "Parents: ~a~%" (map (lambda (h) (substring h 0 12))
                                (block-refs merge)))

;;; ============================================================
;;; Building a Tree (Directory-like Structure)
;;; ============================================================

(display "\n=== Tree Structure ===\n")

(display "
A tree is like a filesystem:

  [root-dir]
    |-- [file1]
    |-- [file2]
    \\-- [subdir]
          |-- [file3]
")

;;; Leaf files
(define file1 (make-block 'file "Contents of file 1" '()))
(define file2 (make-block 'file "Contents of file 2" '()))
(define file3 (make-block 'file "Contents of file 3" '()))
(for-each cas-store! (list file1 file2 file3))

;;; Subdirectory containing file3
(define subdir (make-block 'dir
                           '(("file3" . file))
                           (list (block-hash file3))))
(cas-store! subdir)

;;; Root directory
(define root-dir (make-block 'dir
                             '(("file1" . file)
                               ("file2" . file)
                               ("subdir" . dir))
                             (list (block-hash file1)
                                   (block-hash file2)
                                   (block-hash subdir))))
(cas-store! root-dir)

(format #t "Root dir: ~a...~%" (substring (block-hash root-dir) 0 12))
(format #t "Contains ~a entries~%" (length (block-refs root-dir)))

;;; ============================================================
;;; Properties of Content-Addressed Structures
;;; ============================================================

(display "\n=== Key Properties ===\n")

(display "
1. DETERMINISTIC
   The same structure built twice produces the same hash.
   Regardless of when or where it's built.

2. TAMPER-EVIDENT
   Changing any block changes its hash.
   This changes any block that references it.
   Changes propagate to the root.

3. EFFICIENT COMPARISON
   Two structures are identical iff their root hashes match.
   No need to compare recursively.

4. STRUCTURAL SHARING
   Two similar structures share common sub-structures.
   Only differences need new storage.

5. NATURAL CACHING
   Once you've fetched a hash, cache it forever.
   The content will never change.
")

;;; ============================================================
;;; Demonstrating Structural Sharing
;;; ============================================================

(display "=== Structural Sharing ===\n")

;;; Two trees sharing a common subtree
(define shared-subtree (make-block 'data "This is shared" '()))
(cas-store! shared-subtree)

(define tree1 (make-block 'tree "Tree 1"
                          (list (block-hash shared-subtree))))
(define tree2 (make-block 'tree "Tree 2"
                          (list (block-hash shared-subtree))))
(cas-store! tree1)
(cas-store! tree2)

(format #t "Tree 1: ~a...~%" (substring (block-hash tree1) 0 12))
(format #t "Tree 2: ~a...~%" (substring (block-hash tree2) 0 12))
(format #t "Shared: ~a...~%"
        (substring (car (block-refs tree1)) 0 12))
(format #t "Both reference the same subtree: ~a~%"
        (equal? (car (block-refs tree1))
                (car (block-refs tree2))))

(display "\nDone!\n")
