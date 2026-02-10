(load "core/testing/test-framework.ss")
(load "core/blocks/cas.ss")
(load "boundary/parallel/scheduler.ss")

;;; ============================================================
;;; Thread Safety Tests for CAS Operations
;;; ============================================================

(ensure-pool!)

(test-group "concurrent-store-fetch"

  (define-test "parallel stores do not corrupt hashtable"
    ;; Spawn N concurrent store! calls, verify all blocks survive
    (let* ([n 100]
           [blocks (map (lambda (i)
                          (make-block 'test
                            (string->utf8 (format "block-~a" i))
                            empty-refs))
                        (iota n))]
           [futures (map (lambda (blk)
                           (spawn (lambda () (store! blk))))
                         blocks)]
           [hashes (map await futures)])
      ;; All hashes should be unique and fetchable
      (assert-equal n (length hashes))
      (assert-true (equal? n
        (length (filter (lambda (h) (stored? h)) hashes))))))

  (define-test "parallel fetch during store returns consistent results"
    ;; Store a block, then read and write concurrently
    (let* ([blk (make-block 'stable (string->utf8 "stable-data") empty-refs)]
           [hash (store! blk)]
           [n 50]
           ;; Spawn concurrent fetches + stores
           [fetch-futures (map (lambda (_)
                                 (spawn (lambda () (fetch hash))))
                               (iota n))]
           [store-futures (map (lambda (i)
                                 (spawn (lambda ()
                                          (store! (make-block 'noise
                                            (string->utf8 (format "noise-~a" i))
                                            empty-refs)))))
                               (iota n))]
           [fetch-results (map await fetch-futures)])
      ;; All fetches should return the stable block (not #f)
      (assert-true (equal? n
        (length (filter (lambda (r) (and r (eq? 'stable (block-tag r))))
                        fetch-results))))))

  (define-test "parallel pin and unpin do not race"
    (let* ([blk (make-block 'pintest (string->utf8 "pindata") empty-refs)]
           [hash (store! blk)]
           [n 50]
           ;; Half pin, half unpin, concurrently
           [futures (map (lambda (i)
                           (spawn (lambda ()
                                    (if (even? i)
                                        (pin! hash)
                                        (unpin! hash)))))
                         (iota n))])
      (for-each await futures)
      ;; Should not crash — final state is either pinned or not
      (assert-true (boolean? (pinned? hash)))))

  (define-test "parallel stored? checks are consistent"
    (let* ([blk (make-block 'check (string->utf8 "check-data") empty-refs)]
           [hash (store! blk)]
           [n 50]
           [futures (map (lambda (_)
                           (spawn (lambda () (stored? hash))))
                         (iota n))]
           [results (map await futures)])
      ;; All should return #t
      (assert-true (equal? n (length (filter (lambda (r) (eq? #t r)) results)))))))

(pool-shutdown-global!)
(run-all-tests)
