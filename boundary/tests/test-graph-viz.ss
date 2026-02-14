(doc 'note "Run from project root: scheme --script boundary/tests/test-graph-viz.ss")

(load "core/testing/test-framework.ss")
(load "boundary/ui/graph-viz.ss")

(doc 'module 'test-graph-viz)
(doc 'description "Tests for Graph Visualization")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(display "
==========================================================
              GRAPH VISUALIZATION TESTS
==========================================================
")

(doc 'section 'layout-tests)

(test-group layout-tests
            (define-test layout-get-set-test
              (let* ([h1 (string->utf8 "hash1")]
                     [h2 (string->utf8 "hash2")]
                     [layout '()]
                     [layout1 (layout-set layout h1 (vec2 100 200))]
                     [layout2 (layout-set layout1 h2 (vec2 300 400))])
                    (assert-true (vec2? (layout-get layout2 h1)))
                    (assert-true (vec2? (layout-get layout2 h2)))
                    (assert-false (layout-get layout2 (string->utf8 "unknown")))))
            
            (define-test layout-all-positions-test
              (let* ([h1 (string->utf8 "hash1")]
                     [h2 (string->utf8 "hash2")]
                     [layout (layout-set
                              (layout-set '() h1 (vec2 10 20))
                              h2 (vec2 30 40))]
                     [positions (layout-all-positions layout)])
                    (assert-equal 2 (length positions))
                    (assert-true (for-all vec2? positions)))))

(doc 'section 'tag-color-tests)

(test-group tag-color-tests
            (define-test tag->color-test
              (assert-true (color12? (tag->color 'expr)))
              (assert-true (color12? (tag->color 'lambda)))
              (assert-true (color12? (tag->color 'define)))
              (assert-true (color12? (tag->color 'unknown-tag)))))

(doc 'section 'drawing-helper-tests)

(test-group drawing-helper-tests
            (define-test rad->deg-test
              (let ([deg (rad->deg 3.14159)])
                   ;; Should be close to 180
                   (assert-true (< (abs (- deg 180)) 0.01))))
            
            (define-test goto-test
              (let* ([t (make-turtle)]
                     [t2 (goto t 400 300)])
                    ;; Should be at new position
                    (assert-true (< (abs (- (turtle-x t2) 400)) 1))
                    (assert-true (< (abs (- (turtle-y t2) 300)) 1))))
            
            (define-test draw-circle-test
              (let* ([t (make-turtle)]
                     [t2 (draw-circle t 50)])
                    ;; Should have created paths
                    (assert-true (not (null? (turtle-paths t2)))))))

(doc 'section 'summary)

(print-summary)
