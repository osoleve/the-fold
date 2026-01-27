;;; lattice/data/chase-lev-deque.ss
;;; @module chase-lev-deque
;;; @requires prelude

(load "core/base/prelude.ss")

(doc 'module 'chase-lev-deque)
(doc 'description "Lock-free Chase-Lev work-stealing deque.
Owner pushes/pops from bottom (LIFO), thieves steal from top (FIFO).
Uses monotonic 64-bit indices to avoid ABA problem.")
(doc 'layer 'lattice)
(doc 'tier 0)
(doc 'purity 'partial)  ; Uses mutation for lock-free operations
(doc 'references '("Chase & Lev 2005: Dynamic Circular Work-Stealing Deque"))

;;; Structure: buffer (vector), bottom (box), top (box)
;;; No separate capacity field - thieves compute from (vector-length buffer)

(define (make-chase-lev-deque initial-capacity)
  (doc 'type '(-> Nat ChaselevDeque))
  (doc 'description "Create empty deque with given initial capacity.")
  (list 'chase-lev-deque
        (box (make-vector initial-capacity #f))  ; buffer
        (box 0)                                   ; bottom (monotonic)
        (box 0)))                                 ; top (monotonic)

(define (chase-lev-deque? x)
  (doc 'type '(-> Any Boolean))
  (and (pair? x) (eq? (car x) 'chase-lev-deque)))

(define (deque-buffer d) (unbox (cadr d)))
(define (deque-buffer-box d) (cadr d))
(define (deque-bottom d) (unbox (caddr d)))
(define (deque-bottom-box d) (caddr d))
(define (deque-top d) (unbox (cadddr d)))
(define (deque-top-box d) (cadddr d))

(define (deque-size d)
  (doc 'type '(-> ChaselevDeque Nat))
  (doc 'description "Approximate size (racy but useful for debugging).")
  (max 0 (- (deque-bottom d) (deque-top d))))

(define (deque-empty? d)
  (doc 'type '(-> ChaselevDeque Boolean))
  (doc 'description "Check if deque appears empty.")
  (<= (deque-bottom d) (deque-top d)))
