;;; lattice/data/chase-lev-deque.ss
;;; @module chase-lev-deque
;;; @requires prelude

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)

(doc 'module 'chase-lev-deque)
(doc 'description "Lock-free Chase-Lev work-stealing deque.
Owner pushes/pops from bottom (LIFO), thieves steal from top (FIFO).
Uses monotonic 64-bit indices to avoid ABA problem.")
(doc 'layer 'lattice)
(doc 'tier 0)
(doc 'purity 'partial)  ; Uses mutation for lock-free operations
(doc 'references '("Chase & Lev 2005: Dynamic Circular Work-Stealing Deque"))

;;; Structure: buffer (vector), bottom (box), top (box), generation (box)
;;; Generation counter detects resize-in-progress for thieves

(define (make-chase-lev-deque initial-capacity)
  (doc 'export #t)
  (doc 'type '(-> Nat ChaselevDeque))
  (doc 'description "Create empty deque with given initial capacity.")
  (list 'chase-lev-deque
        (box (make-vector initial-capacity #f))  ; buffer
        (box 0)                                   ; bottom (monotonic)
        (box 0)                                   ; top (monotonic)
        (box 0)))                                 ; generation (incremented on resize)

(define (chase-lev-deque? x)
  (doc 'export #t)
  (doc 'type '(-> Any Boolean))
  (and (pair? x) (eq? (car x) 'chase-lev-deque)))

(define (deque-buffer d) (unbox (cadr d)))
(define (deque-buffer-box d) (cadr d))
(define (deque-bottom d) (unbox (caddr d)))
(define (deque-bottom-box d) (caddr d))
(define (deque-top d) (unbox (cadddr d)))
(define (deque-top-box d) (cadddr d))
(define (deque-generation d) (unbox (list-ref d 4)))
(define (deque-generation-box d) (list-ref d 4))

(define (deque-size d)
  (doc 'export #t)
  (doc 'type '(-> ChaselevDeque Nat))
  (doc 'description "Approximate size (racy but useful for debugging).")
  (max 0 (- (deque-bottom d) (deque-top d))))

(define (deque-empty? d)
  (doc 'export #t)
  (doc 'type '(-> ChaselevDeque Boolean))
  (doc 'description "Check if deque appears empty.")
  (<= (deque-bottom d) (deque-top d)))

(define (deque-push! d task)
  (doc 'export #t)
  (doc 'type '(-> ChaselevDeque Task Void))
  (doc 'description "Owner pushes task to bottom. Resizes if full.")
  (let* ([b (deque-bottom d)]
         [t (deque-top d)]
         [buf (deque-buffer d)]
         [cap (vector-length buf)])
    ;; Resize if full
    (when (>= (- b t) cap)
      (deque-resize! d))
    ;; Write task to buffer
    (let ([buf (deque-buffer d)]  ; Re-read after potential resize
          [cap (vector-length (deque-buffer d))])
      (vector-set! buf (mod b cap) task)
      ;; MEMORY BARRIER: Ensure task is visible before bottom is updated
      ;; Use CAS as a full fence (Chez box-cas! provides sequential consistency)
      (let loop ()
        (let ([cur-b (deque-bottom d)])
          (unless (box-cas! (deque-bottom-box d) cur-b (+ b 1))
            (loop)))))))

(define (deque-resize! d)
  (doc 'type '(-> ChaselevDeque Void))
  (doc 'description "Double buffer capacity, copying valid elements.
Increments generation to signal thieves that buffer changed.")
  (let* ([old-buf (deque-buffer d)]
         [old-cap (vector-length old-buf)]
         [new-cap (* old-cap 2)]
         [new-buf (make-vector new-cap #f)]
         [t (deque-top d)]
         [b (deque-bottom d)]
         [gen (deque-generation d)])
    ;; Copy valid elements [t, b) to new buffer
    (let loop ([i t])
      (when (< i b)
        (vector-set! new-buf (mod i new-cap)
                     (vector-ref old-buf (mod i old-cap)))
        (loop (+ i 1))))
    ;; Increment generation BEFORE swapping buffer (signals resize in progress)
    (set-box! (deque-generation-box d) (+ gen 1))
    ;; MEMORY BARRIER: Use CAS to ensure generation visible before buffer swap
    (let loop ()
      (let ([cur-buf (deque-buffer d)])
        (unless (box-cas! (deque-buffer-box d) cur-buf new-buf)
          (loop))))))

(define (deque-pop! d)
  (doc 'export #t)
  (doc 'type '(-> ChaselevDeque (U Task 'empty)))
  (doc 'description "Owner pops from bottom. Returns 'empty if deque is empty.
Uses CAS for memory barrier and when contending with thieves on last element.")
  (let* ([b (- (deque-bottom d) 1)])
    ;; MEMORY BARRIER: Use CAS to decrement bottom with StoreLoad fence semantics
    ;; This ensures thieves see the decremented bottom before we read top
    (let loop ()
      (let ([cur-b (deque-bottom d)])
        (unless (box-cas! (deque-bottom-box d) cur-b b)
          (loop))))
    (let ([t (deque-top d)])
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
                 'empty)))]))))

(define (deque-steal! d)
  (doc 'export #t)
  (doc 'type '(-> ChaselevDeque (U Task 'empty 'abort)))
  (doc 'description "Thief steals from top. Returns task, 'empty, or 'abort.
Aborts if CAS fails or if generation changed (resize detected).")
  (let* ([gen1 (deque-generation d)]  ; Read generation first
         [t (deque-top d)]
         [b (deque-bottom d)])
    (if (>= t b)
        'empty
        (let* ([buf (deque-buffer d)]
               [cap (vector-length buf)]
               [task (vector-ref buf (mod t cap))]
               [gen2 (deque-generation d)])  ; Check generation again
          ;; Abort if resize happened between reading indices and reading task
          (if (not (= gen1 gen2))
              'abort  ; Resize detected, task may be invalid
              (if (box-cas! (deque-top-box d) t (+ t 1))
                  (begin
                    (vector-set! buf (mod t cap) #f)  ; Clear for GC
                    task)
                  'abort))))))
