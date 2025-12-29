;;; playpen/quill/state.ss — Quill state helpers
;;;
;;; Quill state is a small persistent data structure intended to be:
;;;   - Easy to serialize later (CAS persistence)
;;;   - Easy to validate/inspect
;;;   - Friendly for educational content (progress flags, scores, etc.)
;;;
;;; Representation (alist):
;;;   ((vars  . ((k . v) ...))
;;;    (inv   . (item ...))
;;;    (flags . ((flag . #t/#f) ...))
;;;    (meta  . ((k . v) ...)))

(define (make-quill-state)
  `((vars . ())
    (inv . ())
    (flags . ())
    (meta . ())))

(define (quill-state-section state key)
  (let ([p (assq key state)])
       (if p (cdr p) '())))

(define (quill-state-set-section state key value)
  (let ([p (assq key state)])
       (if p
           (let ([rest (remq p state)])
                (cons (cons key value) rest))
           (cons (cons key value) state))))

;;; ------------------------------------------------------------
;;; Vars
;;; ------------------------------------------------------------

(define (quill-state-var state key)
  (let ([vars (quill-state-section state 'vars)])
       (let ([p (assq key vars)])
            (and p (cdr p)))))

(define (quill-state-set-var state key value)
  (let* ([vars (quill-state-section state 'vars)]
         [p (assq key vars)]
         [vars2 (if p
                    (cons (cons key value) (remq p vars))
                    (cons (cons key value) vars))])
        (quill-state-set-section state 'vars vars2)))

(define (quill-state-inc-var state key delta)
  (let* ([cur (or (quill-state-var state key) 0)]
         [next (+ cur delta)])
        (quill-state-set-var state key next)))

;;; ------------------------------------------------------------
;;; Flags
;;; ------------------------------------------------------------

(define (quill-state-flag? state flag)
  (let ([flags (quill-state-section state 'flags)])
       (let ([p (assq flag flags)])
            (and p (cdr p)))))

(define (quill-state-set-flag state flag value)
  (let* ([flags (quill-state-section state 'flags)]
         [p (assq flag flags)]
         [flags2 (if p
                     (cons (cons flag value) (remq p flags))
                     (cons (cons flag value) flags))])
        (quill-state-set-section state 'flags flags2)))

(define (quill-state-flag state flag)
  (quill-state-set-flag state flag #t))

(define (quill-state-unflag state flag)
  (quill-state-set-flag state flag #f))

;;; ------------------------------------------------------------
;;; Inventory
;;; ------------------------------------------------------------

(define (quill-state-inventory state)
  (quill-state-section state 'inv))

(define (quill-state-has-item? state item)
  (and (memq item (quill-state-inventory state)) #t))

(define (quill-state-add-item state item)
  (let ([inv (quill-state-inventory state)])
       (if (memq item inv)
           state
           (quill-state-set-section state 'inv (cons item inv)))))

(define (quill-state-remove-item state item)
  (let ([inv (quill-state-inventory state)])
       (quill-state-set-section state 'inv (remq item inv))))
