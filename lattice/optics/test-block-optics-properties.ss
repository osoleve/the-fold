;;; lattice/optics/test-block-optics-properties.ss — QuickCheck properties for block optics

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'optics)
(require 'block-optics)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define (mk-ref n)
  (let ([bv (make-bytevector 33 0)])
    (bytevector-u8-set! bv 0 (modulo (abs n) 256))
    bv))

(define (mk-block tag-int payload-int refs-int)
  (let ([tag (if (even? tag-int) 'lit 'app)]
        [payload (string->utf8 (number->string payload-int))]
        [refs (vector (mk-ref refs-int) (mk-ref (+ refs-int 1)))])
    (make-block tag payload refs)))

(define (refs-equal? a b)
  (let ([n (vector-length a)])
    (and (= n (vector-length b))
         (let loop ([i 0])
           (or (= i n)
               (and (bytevector=? (vector-ref a i) (vector-ref b i))
                    (loop (+ i 1))))))))

(define (gen-block)
  (gen-bind (gen-int-range -1000 1000)
    (lambda (a)
      (gen-bind (gen-int-range -1000 1000)
        (lambda (b)
          (gen-map (lambda (c) (mk-block a b c))
                   (gen-int-range -1000 1000)))))))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group block-optics-properties

  (define-property "block-tag-lens put-get"
    (gen-bind (gen-int-range 0 1)
      (lambda (t)
        (gen-map (lambda (blk) (cons t blk)) (gen-block))))
    (lambda (pair)
      (let* ([tag (if (= (car pair) 0) 'lit 'custom)]
             [blk (cdr pair)]
             [updated (set-lens block-tag-lens tag blk)])
        (equal? tag (view block-tag-lens updated))))
    'tests 200)

  (define-property "block-payload-lens get-put"
    (gen-block)
    (lambda (blk)
      (let ([blk2 (set-lens block-payload-lens (view block-payload-lens blk) blk)])
        (and (equal? (block-tag blk2) (block-tag blk))
             (bytevector=? (block-payload blk2) (block-payload blk))
             (refs-equal? (block-refs blk2) (block-refs blk)))))
    'tests 180)

  (define-property "block-ref-at valid index focuses first ref"
    (gen-block)
    (lambda (blk)
      (let ([mref (affine-preview (block-ref-at 0) blk)])
        (and (just? mref)
             (bytevector=? (from-just mref)
                           (vector-ref (block-refs blk) 0)))))
    'tests 180)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
