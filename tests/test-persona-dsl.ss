;;; test-persona-dsl.ss - Unit tests for persona prompt DSL
(load "agents/lib/persona-prompt-gen.ss")

(define (assert-equal? actual expected msg)
  (if (equal? actual expected)
      (display (string-append "PASS: " msg "\n"))
      (error 'test (string-append "FAIL: " msg " - Expected " (format "~a" expected) ", got " (format "~a" actual)))))

(define (test-interpolate)
  (display "\n=== Testing Interpolate ===\n")
  (assert-equal? (interpolate "Hello {name}!" 'name "World")
                 "Hello World!"
                 "Basic interpolation")
  (assert-equal? (interpolate "{a} + {b} = {c}" 'a "1" 'b "2" 'c "3")
                 "1 + 2 = 3"
                 "Multiple variables")
  (assert-equal? (interpolate "No vars here")
                 "No vars here"
                 "No variables")
  (assert-equal? (interpolate "Repeated {x} {x}" 'x "val")
                 "Repeated val val"
                 "Repeated variables")
  (assert-equal? (interpolate "Unused {x}" 'x "val" 'y "unused")
                 "Unused val"
                 "Extra arguments ignored"))

(define (test-string-replace)
  (display "\n=== Testing String Replace ===\n")
  (assert-equal? (string-replace "hello world" "world" "scheme")
                 "hello scheme"
                 "Basic replacement")
  (assert-equal? (string-replace "banana" "na" "XY")
                 "baXYXY"
                 "Multiple replacements")
  (assert-equal? (string-replace "no match" "foo" "bar")
                 "no match"
                 "No match"))

(define (test-weighted-choice)
  (display "\n=== Testing Weighted Choice (Probabilistic) ===\n")
  (let* ([samples 1000]
         [results (make-eq-hashtable)]
         [opts `((80 "A") (20 "B"))])
        
        (do ([i 0 (+ i 1)]) ((= i samples))
            (let ([res (apply weighted-choice opts)])
                 (hashtable-set! results (string->symbol res)
                                 (+ 1 (hashtable-ref results (string->symbol res) 0)))))
        
        (let ([count-a (hashtable-ref results 'A 0)]
              [count-b (hashtable-ref results 'B 0)])
             (display (format "A: ~a (~a%)\n" count-a (* 100.0 (/ count-a samples))))
             (display (format "B: ~a (~a%)\n" count-b (* 100.0 (/ count-b samples))))
             
             (if (and (> count-a 700) (< count-a 900))
                 (display "PASS: Weighted distribution within expected range (80% +/- 10%)\n")
                 (display "WARN: Weighted distribution outside typical variance (check manual)\n")))))

(define (test-maybe)
  (display "\n=== Testing Maybe (Probabilistic) ===\n")
  (let* ([samples 1000]
         [count-present 0])
        (do ([i 0 (+ i 1)]) ((= i samples))
            (when (string=? (maybe "yes" 0.3) "yes")
                  (set! count-present (+ count-present 1))))
        
        (display (format "Present: ~a (~a%)\n" count-present (* 100.0 (/ count-present samples))))
        
        (if (and (> count-present 250) (< count-present 350))
            (display "PASS: Maybe distribution within expected range (30% +/- 5%)\n")
            (display "WARN: Maybe distribution outside typical variance (check manual)\n"))))

(define (test-context)
  (display "\n=== Testing Context Injection ===\n")
  ;; Test default/env var fallback
  (assert-equal? (context-ref 'NON_EXISTENT "default") "default" "Default value works")
  
  ;; Test with-context
  (with-context '((TEST_VAR . "injected"))
                (lambda ()
                        (assert-equal? (context-ref 'TEST_VAR) "injected" "Context injection works")))
  
  ;; Ensure context is reset
  (assert-equal? (context-ref 'TEST_VAR "gone") "gone" "Context resets after with-context"))

(define (test-fragments)
  (display "\n=== Testing Namespaced Fragments ===\n")
  (let ([style (fragment (warm "Warm Text") (cool "Cool Text"))])
       (assert-equal? (fragment-ref style 'warm) "Warm Text" "Fragment access works")
       (assert-equal? (fragment-ref style 'cool) "Cool Text" "Multiple keys work")
       (assert-equal? (fragment-ref style 'missing "default") "default" "Fragment default works")))

;; Run tests
(test-string-replace)
(test-interpolate)
(test-weighted-choice)
(test-maybe)
(test-context)
(test-fragments)
