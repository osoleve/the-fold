;;; @module contracts-chaperone
;;; @description Chaperone-style contracts for higher-order elements in collections.
;;; Loaded by contracts.ss — requires contract core, blame, and wrap infrastructure.

(doc 'section 'chaperone-contracts)

;;; Chaperone-style contracts for higher-order elements in collections.
;;; Unlike flat contracts that check immediately, chaperones wrap the
;;; collection and apply element contracts lazily when elements are accessed.
;;;
;;; OPTIMIZATION (fold-zxva): Uses shared-spine representation to reduce
;;; allocation from O(4N) to O(3N + 4) cons cells during iteration.
;;; The spine (original list, contract, location) is shared across all
;;; chaperone positions, so chaperone-cdr only allocates 3 cons cells
;;; instead of 4 per step.

;;; Internal: Spine shared by all positions in a chaperoned list
;;; Spine stores: (chaperone-spine original len contract location)
;;; Length is cached to avoid O(N) computation on every cdr call.
(define (chaperone-spine? x)
  ;; O(1) structural check for 5-element spine
  (and (pair? x)
       (eq? (car x) 'chaperone-spine)
       (pair? (cdr x))      ; original
       (pair? (cddr x))     ; len
       (pair? (cdddr x))    ; contract
       (pair? (cddddr x))   ; location
       (null? (cdr (cddddr x)))))

(define (make-chaperone-spine original contract location)
  (doc 'type (-> List Contract Symbol ChaperoneSpine))
  (doc 'description "Create a shared spine for chaperone list positions.")
  (doc 'export #f)
  ;; Cache the length to avoid O(N) lookup on every cdr
  `(chaperone-spine ,original ,(length original) ,contract ,location))

(define (chaperone-spine-original spine)
  (cadr spine))

(define (chaperone-spine-length spine)
  (caddr spine))

(define (chaperone-spine-contract spine)
  (cadddr spine))

(define (chaperone-spine-location spine)
  (car (cddddr spine)))

;;; External: chaperone-list is (chaperone-list index spine)
;;; Index is the current position in the original list

(define (chaperone-list? x)
  (doc 'type (-> Any Boolean))
  (doc 'description "Check if value is a chaperoned list. O(1) structural check.")
  (doc 'export #t)
  ;; Structural check: (chaperone-list index spine) where spine is valid
  ;; Avoid (length x) which is O(N); instead check structure directly
  (and (pair? x)
       (eq? (car x) 'chaperone-list)
       (pair? (cdr x))          ; has at least index
       (integer? (cadr x))
       (pair? (cddr x))         ; has spine
       (chaperone-spine? (caddr x))
       (null? (cdddr x))))

(define (make-chaperone-list underlying elem-contract location)
  (doc 'type (-> List Contract Symbol ChaperoneList))
  (doc 'description "Create a chaperoned list that applies elem-contract on access.")
  (doc 'export #f)
  ;; Create spine from the full list, starting at index 0
  `(chaperone-list 0 ,(make-chaperone-spine underlying elem-contract location)))

;; Internal constructor for advancing position (reuses existing spine)
(define (make-chaperone-list-at-index index spine)
  `(chaperone-list ,index ,spine))

;; Wire up forward reference
(set! *make-chaperone-list* make-chaperone-list)

(define (chaperone-list-index cl)
  (if (chaperone-list? cl) (cadr cl) 0))

(define (chaperone-list-spine cl)
  (if (chaperone-list? cl) (caddr cl) #f))

(define (chaperone-list-underlying cl)
  (doc 'type (-> ChaperoneList List))
  (doc 'description "Get the remaining underlying list from current position.")
  (doc 'export #f)
  (if (chaperone-list? cl)
      (let ([spine (chaperone-list-spine cl)]
            [idx (chaperone-list-index cl)])
        (list-tail (chaperone-spine-original spine) idx))
      cl))

(define (chaperone-list-contract cl)
  (if (chaperone-list? cl)
      (chaperone-spine-contract (chaperone-list-spine cl))
      any/c))

(define (chaperone-list-location cl)
  (if (chaperone-list? cl)
      (chaperone-spine-location (chaperone-list-spine cl))
      'unknown))

(define (chaperone-car cl)
  (doc 'type (-> ChaperoneList Any))
  (doc 'description "Get first element of chaperoned list, applying element contract.")
  (doc 'export #t)
  (if (chaperone-list? cl)
      (let* ([spine (chaperone-list-spine cl)]
             [idx (chaperone-list-index cl)]
             [original (chaperone-spine-original spine)]
             [elem-contract (chaperone-spine-contract spine)]
             [location (chaperone-spine-location spine)]
             [elem (list-ref original idx)])
        (apply-contract elem-contract elem location))
      (car cl)))

(define (chaperone-cdr cl)
  (doc 'type (-> ChaperoneList ChaperoneList))
  (doc 'description "Get rest of chaperoned list, preserving chaperone. O(1) via shared spine with cached length.")
  (doc 'export #t)
  (if (chaperone-list? cl)
      (let* ([spine (chaperone-list-spine cl)]
             [idx (chaperone-list-index cl)]
             [len (chaperone-spine-length spine)]  ; O(1) cached lookup
             [new-idx (+ idx 1)])
        (if (>= new-idx len)
            '()
            ;; Only allocate 3 cons cells, reusing the spine
            (make-chaperone-list-at-index new-idx spine)))
      (cdr cl)))

(define (chaperone-list-ref cl n)
  (doc 'type (-> ChaperoneList Nat Any))
  (doc 'description "Get nth element of chaperoned list, applying element contract.")
  (doc 'export #t)
  (if (chaperone-list? cl)
      (let* ([spine (chaperone-list-spine cl)]
             [idx (chaperone-list-index cl)]
             [original (chaperone-spine-original spine)]
             [elem-contract (chaperone-spine-contract spine)]
             [location (chaperone-spine-location spine)]
             [elem (list-ref original (+ idx n))])
        (apply-contract elem-contract elem location))
      (list-ref cl n)))

(define (chaperone-list->list cl)
  (doc 'type (-> ChaperoneList List))
  (doc 'description "Convert chaperoned list to regular list, checking all elements.")
  (doc 'export #t)
  (if (chaperone-list? cl)
      (let* ([spine (chaperone-list-spine cl)]
             [idx (chaperone-list-index cl)]
             [original (chaperone-spine-original spine)]
             [elem-contract (chaperone-spine-contract spine)]
             [location (chaperone-spine-location spine)]
             [remaining (list-tail original idx)])
        (map (lambda (elem) (apply-contract elem-contract elem location))
             remaining))
      cl))

(define (chaperone-list-length cl)
  (doc 'type (-> ChaperoneList Nat))
  (doc 'description "Get length of chaperoned list from current position. O(1) via cached length.")
  (doc 'export #t)
  (if (chaperone-list? cl)
      (let* ([spine (chaperone-list-spine cl)]
             [idx (chaperone-list-index cl)]
             [len (chaperone-spine-length spine)])  ; O(1) cached lookup
        (- len idx))
      (length cl)))

(define (chaperone-list-null? cl)
  (doc 'type (-> ChaperoneList Boolean))
  (doc 'description "Check if chaperoned list is empty. O(1) via cached length.")
  (doc 'export #t)
  (if (chaperone-list? cl)
      (let* ([spine (chaperone-list-spine cl)]
             [idx (chaperone-list-index cl)]
             [len (chaperone-spine-length spine)])  ; O(1) cached lookup
        (>= idx len))
      (null? cl)))

;;; Chaperone vectors

(define (chaperone-vector? x)
  (doc 'type (-> Any Boolean))
  (doc 'description "Check if value is a chaperoned vector.")
  (doc 'export #t)
  (and (pair? x)
       (eq? (car x) 'chaperone-vector)))

(define (make-chaperone-vector underlying elem-contract location)
  (doc 'type (-> Vector Contract Symbol ChaperoneVector))
  (doc 'description "Create a chaperoned vector that applies elem-contract on access.")
  (doc 'export #f)
  `(chaperone-vector ,underlying ,elem-contract ,location))

;; Wire up forward reference
(set! *make-chaperone-vector* make-chaperone-vector)

(define (chaperone-vector-underlying cv)
  (if (chaperone-vector? cv) (cadr cv) cv))

(define (chaperone-vector-contract cv)
  (if (chaperone-vector? cv) (caddr cv) any/c))

(define (chaperone-vector-location cv)
  (if (chaperone-vector? cv) (cadddr cv) 'unknown))

(define (chaperone-vector-ref cv n)
  (doc 'type (-> ChaperoneVector Nat Any))
  (doc 'description "Get nth element of chaperoned vector, applying element contract.")
  (doc 'export #t)
  (if (chaperone-vector? cv)
      (let* ([underlying (chaperone-vector-underlying cv)]
             [elem-contract (chaperone-vector-contract cv)]
             [location (chaperone-vector-location cv)]
             [elem (vector-ref underlying n)])
        (apply-contract elem-contract elem location))
      (vector-ref cv n)))

(define (chaperone-vector-length cv)
  (doc 'type (-> ChaperoneVector Nat))
  (doc 'description "Get length of chaperoned vector.")
  (doc 'export #t)
  (vector-length (if (chaperone-vector? cv)
                     (chaperone-vector-underlying cv)
                     cv)))

(define (chaperone-vector->list cv)
  (doc 'type (-> ChaperoneVector List))
  (doc 'description "Convert chaperoned vector to list, checking all elements.")
  (doc 'export #t)
  (if (chaperone-vector? cv)
      (let ([underlying (chaperone-vector-underlying cv)]
            [elem-contract (chaperone-vector-contract cv)]
            [location (chaperone-vector-location cv)]
            [len (vector-length (chaperone-vector-underlying cv))])
        (let loop ([i 0] [acc '()])
          (if (>= i len)
              (reverse acc)
              (loop (+ i 1)
                    (cons (apply-contract elem-contract
                                         (vector-ref underlying i)
                                         location)
                          acc)))))
      (vector->list cv)))

;;; Smart listof/vectorof that auto-detect when chaperones are needed

(define (needs-chaperone? elem-contract)
  (doc 'type (-> Contract Boolean))
  (doc 'description "Check if element contract requires chaperone wrapping.")
  (doc 'export #f)
  (or (function-contract? elem-contract)
      (dependent-contract? elem-contract)
      ;; Also check for nested listof/vectorof with HO contracts
      (and (pair? elem-contract)
           (memq (car elem-contract) '(chaperone-listof chaperone-vectorof)))))

(define (listof/c elem-contract)
  (doc 'type (-> Contract Contract))
  (doc 'description "Smart list contract: uses chaperone for HO contracts, flat otherwise.")
  (doc 'export #t)
  (if (needs-chaperone? elem-contract)
      `(chaperone-listof ,elem-contract)
      (listof elem-contract)))

(define (vectorof/c elem-contract)
  (doc 'type (-> Contract Contract))
  (doc 'description "Smart vector contract: uses chaperone for HO contracts, flat otherwise.")
  (doc 'export #t)
  (if (needs-chaperone? elem-contract)
      `(chaperone-vectorof ,elem-contract)
      (vectorof elem-contract)))

;;; Extend contract-wrap to handle chaperone contracts

(define (chaperone-listof-contract? c)
  (and (pair? c) (eq? (car c) 'chaperone-listof)))

(define (chaperone-vectorof-contract? c)
  (and (pair? c) (eq? (car c) 'chaperone-vectorof)))

(define (contract-wrap-chaperone contract value location)
  (doc 'type (-> Contract Any Symbol (Result Any Blame)))
  (doc 'description "Extended contract-wrap that handles chaperone contracts.")
  (doc 'export #t)
  (cond
   ;; Chaperone listof
   [(chaperone-listof-contract? contract)
    (let ([elem-contract (cadr contract)])
      (if (list? value)
          `(Ok ,(make-chaperone-list value elem-contract location))
          `(Err ,(make-blame 'callee location
                             "Expected a list for listof contract"
                             value))))]
   ;; Chaperone vectorof
   [(chaperone-vectorof-contract? contract)
    (let ([elem-contract (cadr contract)])
      (if (vector? value)
          `(Ok ,(make-chaperone-vector value elem-contract location))
          `(Err ,(make-blame 'callee location
                             "Expected a vector for vectorof contract"
                             value))))]
   ;; Fall through to standard contract-wrap
   [else
    (contract-wrap contract value location)]))

(define (apply-contract/c contract value location)
  (doc 'type (-> Contract Any Symbol Any))
  (doc 'description "Apply contract with chaperone support, raise error on violation.")
  (doc 'export #t)
  (let ([result (contract-wrap-chaperone contract value location)])
    (if (eq? (car result) 'Err)
        (raise-contract-violation/blame! (cadr result))
        (cadr result))))

