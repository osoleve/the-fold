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

(define (deque-push! d task)
  (doc 'type '(-> ChaselevDeque Task Void))
  (doc 'description "Owner pushes task to bottom. Resizes if full.")
  (let* ([b (deque-bottom d)]
         [t (deque-top d)]
         [buf (deque-buffer d)]
         [cap (vector-length buf)])
    ;; Resize if full
    (when (>= (- b t) cap)
      (deque-resize! d))
    ;; Write task and increment bottom
    (let ([buf (deque-buffer d)]  ; Re-read after potential resize
          [cap (vector-length (deque-buffer d))])
      (vector-set! buf (mod b cap) task)
      (set-box! (deque-bottom-box d) (+ b 1)))))

(define (deque-resize! d)
  (doc 'type '(-> ChaselevDeque Void))
  (doc 'description "Double buffer capacity, copying valid elements.")
  (let* ([old-buf (deque-buffer d)]
         [old-cap (vector-length old-buf)]
         [new-cap (* old-cap 2)]
         [new-buf (make-vector new-cap #f)]
         [t (deque-top d)]
         [b (deque-bottom d)])
    ;; Copy valid elements [t, b) to new buffer
    (let loop ([i t])
      (when (< i b)
        (vector-set! new-buf (mod i new-cap)
                     (vector-ref old-buf (mod i old-cap)))
        (loop (+ i 1))))
    (set-box! (deque-buffer-box d) new-buf)))

(define (deque-pop! d)
  (doc 'type '(-> ChaselevDeque (U Task 'empty)))
  (doc 'description "Owner pops from bottom. Returns 'empty if deque is empty.
Uses CAS when contending with thieves on last element.")
  (let* ([b (- (deque-bottom d) 1)]
         [_ (set-box! (deque-bottom-box d) b)]  ; Decrement first
         [t (deque-top d)])
    (cond
      [(< b t)
       ;; Queue was empty, restore bottom
       (set-box! (deque-bottom-box d) t)
       'empty]
      [(> b t)
       ;; Multiple elements, safe to take
       (let* ([buf (deque-buffer d)]
              [cap (vector-length buf)]
              [task (vector-ref buf (mod b cap))])
         (vector-set! buf (mod b cap) #f)  ; Clear for GC
         task)]
      [else
       ;; b == t: Last element, race with thieves
       (let* ([buf (deque-buffer d)]
              [cap (vector-length buf)]
              [task (vector-ref buf (mod b cap))])
         (if (box-cas! (deque-top-box d) t (+ t 1))
             ;; Won the race
             (begin
               (set-box! (deque-bottom-box d) (+ t 1))
               (vector-set! buf (mod b cap) #f)
               task)
             ;; Lost to thief
             (begin
               (set-box! (deque-bottom-box d) (+ t 1))
               'empty)))])))
