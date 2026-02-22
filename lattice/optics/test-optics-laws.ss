;;; lattice/optics/test-optics-laws.ss — QuickCheck property tests for optics laws

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'qc-laws)
(require 'optics)

;;; ============================================================================
;;; Generators
;;; ============================================================================

;; Pairs for lens-fst / lens-snd testing
(define gen-int-pair
  (gen-pair gen-int gen-int))

;; Non-empty lists for lens-head / lens-tail / lens-nth
(define gen-nonempty-int-list
  (gen-bind (gen-int-range 1 10)
    (lambda (n) (gen-list-of n gen-int))))

;; Maybe values for prism-just
(define gen-maybe-int
  (gen-one-of (list (gen-pure nothing)
                    (gen-map just gen-int))))

;; Either values for prism-left / prism-right
(define gen-either-int
  (gen-one-of (list (gen-map left gen-int)
                    (gen-map right gen-int))))

;; Lists (possibly empty) for prism-cons / prism-nil
(define gen-int-list
  (gen-list gen-int))

;;; ============================================================================
;;; Lens law checkers
;;; ============================================================================

(define (check-lens-get-put gen-s lens =?)
  ;; Get-Put: set l (view l s) s = s
  (check-property gen-s
    (lambda (s) (=? (set-lens lens (view lens s) s) s))
    'tests 200))

(define (check-lens-put-get gen-s gen-a lens =?)
  ;; Put-Get: view l (set l a s) = a
  (check-property (gen-pair gen-s gen-a)
    (lambda (sa)
      (let ([s (car sa)] [a (cdr sa)])
        (=? (view lens (set-lens lens a s)) a)))
    'tests 200))

(define (check-lens-put-put gen-s gen-a lens =?)
  ;; Put-Put: set l a' (set l a s) = set l a' s
  (check-property (gen-bind gen-s (lambda (s)
                   (gen-bind gen-a (lambda (a)
                     (gen-map (lambda (a2) (list s a a2)) gen-a)))))
    (lambda (args)
      (let ([s (car args)] [a (cadr args)] [a2 (caddr args)])
        (=? (set-lens lens a2 (set-lens lens a s))
            (set-lens lens a2 s))))
    'tests 200))

;;; ============================================================================
;;; Iso law tests
;;; ============================================================================

(test-group iso-laws

  (define-property "iso-id roundtrip forward"
    gen-int
    (lambda (x) (= (iso-view iso-id x) x))
    'tests 100)

  (define-property "iso-id roundtrip backward"
    gen-int
    (lambda (x) (= (iso-review iso-id x) x))
    'tests 100)

  (define-test "iso-swapped roundtrip"
    (let ([result (check-involution-law gen-int-pair
                    (lambda (p) (iso-view iso-swapped p))
                    equal?)])
      (assert-true (qc-success? result))))

  (define-property "iso-reversed roundtrip (forward then backward)"
    gen-int-list
    (lambda (lst)
      (equal? ((compose2 (iso-backward iso-reversed)
                          (iso-forward iso-reversed)) lst)
              lst))
    'tests 200)

  (define-property "iso-reversed roundtrip (backward then forward)"
    gen-int-list
    (lambda (lst)
      (equal? ((compose2 (iso-forward iso-reversed)
                          (iso-backward iso-reversed)) lst)
              lst))
    'tests 200)

  (define-property "iso-compose roundtrip"
    gen-int-pair
    (lambda (p)
      (let ([composed (iso-compose iso-swapped iso-swapped)])
        (equal? (iso-view composed p) p)))
    'tests 100)
)

;;; ============================================================================
;;; Lens law tests — lens-fst
;;; ============================================================================

(test-group lens-fst-laws

  (define-test "lens-fst Get-Put"
    (assert-true (qc-success? (check-lens-get-put gen-int-pair lens-fst equal?))))

  (define-test "lens-fst Put-Get"
    (assert-true (qc-success? (check-lens-put-get gen-int-pair gen-int lens-fst equal?))))

  (define-test "lens-fst Put-Put"
    (assert-true (qc-success? (check-lens-put-put gen-int-pair gen-int lens-fst equal?))))
)

;;; ============================================================================
;;; Lens law tests — lens-snd
;;; ============================================================================

(test-group lens-snd-laws

  (define-test "lens-snd Get-Put"
    (assert-true (qc-success? (check-lens-get-put gen-int-pair lens-snd equal?))))

  (define-test "lens-snd Put-Get"
    (assert-true (qc-success? (check-lens-put-get gen-int-pair gen-int lens-snd equal?))))

  (define-test "lens-snd Put-Put"
    (assert-true (qc-success? (check-lens-put-put gen-int-pair gen-int lens-snd equal?))))
)

;;; ============================================================================
;;; Lens law tests — lens-head
;;; ============================================================================

(test-group lens-head-laws

  (define-test "lens-head Get-Put"
    (assert-true (qc-success? (check-lens-get-put gen-nonempty-int-list lens-head equal?))))

  (define-test "lens-head Put-Get"
    (assert-true (qc-success? (check-lens-put-get gen-nonempty-int-list gen-int lens-head equal?))))

  (define-test "lens-head Put-Put"
    (assert-true (qc-success? (check-lens-put-put gen-nonempty-int-list gen-int lens-head equal?))))
)

;;; ============================================================================
;;; Lens law tests — lens-id
;;; ============================================================================

(test-group lens-id-laws

  (define-test "lens-id Get-Put"
    (assert-true (qc-success? (check-lens-get-put gen-int lens-id equal?))))

  (define-test "lens-id Put-Get"
    (assert-true (qc-success? (check-lens-put-get gen-int gen-int lens-id equal?))))

  (define-test "lens-id Put-Put"
    (assert-true (qc-success? (check-lens-put-put gen-int gen-int lens-id equal?))))
)

;;; ============================================================================
;;; Lens composition laws
;;; ============================================================================

(test-group lens-compose-laws

  ;; lens-compose lens-fst lens-fst on ((a . b) . c)
  (define gen-nested-pair
    (gen-bind gen-int (lambda (a)
      (gen-bind gen-int (lambda (b)
        (gen-map (lambda (c) (cons (cons a b) c)) gen-int))))))

  (define composed-fst-fst (lens-compose lens-fst lens-fst))

  (define-test "composed lens Get-Put"
    (assert-true (qc-success? (check-lens-get-put gen-nested-pair composed-fst-fst equal?))))

  (define-test "composed lens Put-Get"
    (assert-true (qc-success? (check-lens-put-get gen-nested-pair gen-int composed-fst-fst equal?))))

  (define-test "composed lens Put-Put"
    (assert-true (qc-success? (check-lens-put-put gen-nested-pair gen-int composed-fst-fst equal?))))
)

;;; ============================================================================
;;; Prism law tests — prism-just
;;; ============================================================================

(test-group prism-just-laws

  ;; Preview-Review: preview p (review p a) = Just a
  (define-property "prism-just Preview-Review"
    gen-int
    (lambda (a)
      (let ([result (preview prism-just (review prism-just a))])
        (and (just? result)
             (= (from-just result) a))))
    'tests 200)

  ;; When preview succeeds, review reconstitutes
  (define-property "prism-just Review-Preview"
    gen-maybe-int
    (lambda (m)
      (let ([result (preview prism-just m)])
        (if (nothing? result)
            #t  ; law is vacuously true when preview fails
            (equal? (review prism-just (from-just result)) m))))
    'tests 200)
)

;;; ============================================================================
;;; Prism law tests — prism-cons
;;; ============================================================================

(test-group prism-cons-laws

  (define-property "prism-cons Preview-Review"
    (gen-pair gen-int gen-int-list)
    (lambda (ht)
      (let* ([built (review prism-cons ht)]
             [result (preview prism-cons built)])
        (and (just? result)
             (equal? (from-just result) ht))))
    'tests 200)

  (define-property "prism-cons Review-Preview on non-empty"
    gen-nonempty-int-list
    (lambda (lst)
      (let ([result (preview prism-cons lst)])
        (if (nothing? result)
            #t
            (equal? (review prism-cons (from-just result)) lst))))
    'tests 200)
)

;;; ============================================================================
;;; Prism law tests — prism-left / prism-right
;;; ============================================================================

(test-group prism-left-right-laws

  (define-property "prism-left Preview-Review"
    gen-int
    (lambda (a)
      (let ([result (preview prism-left (review prism-left a))])
        (and (just? result)
             (= (from-just result) a))))
    'tests 200)

  (define-property "prism-right Preview-Review"
    gen-int
    (lambda (b)
      (let ([result (preview prism-right (review prism-right b))])
        (and (just? result)
             (= (from-just result) b))))
    'tests 200)

  (define-property "prism-left and prism-right are disjoint"
    gen-either-int
    (lambda (e)
      ;; Exactly one of left/right preview succeeds
      (let ([l (preview prism-left e)]
            [r (preview prism-right e)])
        (or (and (just? l) (nothing? r))
            (and (nothing? l) (just? r)))))
    'tests 200)
)

;;; ============================================================================
;;; over (lens) satisfies the expected property
;;; ============================================================================

(test-group over-properties

  (define-property "over lens-fst with f is equivalent to manual"
    (gen-pair gen-int gen-int)
    (lambda (p)
      (let ([f (lambda (x) (+ x 1))])
        (equal? (over lens-fst f p)
                (cons (f (car p)) (cdr p)))))
    'tests 200)

  (define-property "over lens-snd with f is equivalent to manual"
    (gen-pair gen-int gen-int)
    (lambda (p)
      (let ([f (lambda (x) (* x 2))])
        (equal? (over lens-snd f p)
                (cons (car p) (f (cdr p))))))
    'tests 200)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
