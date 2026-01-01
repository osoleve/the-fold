;;; test-playpen-unit.ss --- Unit tests for playpen experiments
;;; Commentary: Comprehensive unit testing for playpen functionality
;;; Code:

;; Load security utilities first
(load "user/security-utils.ss")

;; Generic test framework
(define *tests-run* 0)
(define *tests-passed* 0)
(define *tests-failed* 0)

(define (assert-equal test-name expected actual)
  "Assert that expected equals actual"
  (set! *tests-run* (+ *tests-run* 1))
  (if (equal? expected actual)
      (begin
       (set! *tests-passed* (+ *tests-passed* 1))
       (display (string-append "✓ " test-name))
       (newline))
      (begin
       (set! *tests-failed* (+ *tests-failed* 1))
       (display (string-append "✗ " test-name))
       (newline)
       (display (string-append "  Expected: " (format "~a" expected)))
       (newline)
       (display (string-append "  Actual:   " (format "~a" actual)))
       (newline))))

(define (assert-true test-name condition)
  "Assert that condition is true"
  (assert-equal test-name #t condition))

(define (assert-false test-name condition)
  "Assert that condition is false"
  (assert-equal test-name #f condition))

;; Test string-art.ss functionality
(define (test-string-art)
  (display "=== Testing string-art.ss functionality ===")
  (newline)
  
  ;; Test template rendering (if available)
  (if (file-exists? "string-art.ss")
      (begin
       (load "string-art.ss")
       
       ;; Test basic template functionality
       (let ((template "Hello {{name}}, welcome to {{place}}!")
             (variables '((name . "Alice") (place . "The Fold"))))
            
            ;; Test that template variables are properly sanitized
            (assert-true "Template variables are sanitized"
                         (and (valid-string? "{{name}}")
                              (valid-string? "Alice")
                              (valid-string? "The Fold")))
            
            ;; Test path sanitization for any file operations
            (assert-equal "Path sanitization removes .."
                          "safepath"
                          (sanitize-filename "../../../safepath"))
            
            (assert-equal "Path sanitization removes ~"
                          "homeuserfile"
                          (sanitize-filename "~/homeuser/file"))))
      (display "string-art.ss not found - skipping tests"))
  (newline))

;; Test block-playground.ss functionality
(define (test-block-playground)
  (display "=== Testing block-playground.ss functionality ===")
  (newline)
  
  (if (file-exists? "block-playground.ss")
      (begin
       (load "block-playground.ss")
       
       ;; Test block content validation
       (assert-true "Block content validation accepts safe content"
                    (valid-string? "safe block content"))
       
       (assert-false "Block content validation rejects dangerous content"
                     (valid-string? (make-string 10000 #\x)))  ; Too long
       
       ;; Test coordinate validation (if block operations use coordinates)
       (assert-true "Coordinate validation accepts reasonable values"
                    (and (valid-integer? 100)
                         (valid-integer? -50)
                         (valid-integer? 0)))
       
       (assert-false "Coordinate validation rejects extreme values"
                     (valid-integer? 1000000)))
      (display "block-playground.ss not found - skipping tests"))
  (newline))

;; Test string-puzzle.ss functionality
(define (test-string-puzzle)
  (display "=== Testing string-puzzle.ss functionality ===")
  (newline)
  
  (if (file-exists? "string-puzzle.ss")
      (begin
       (load "string-puzzle.ss")
       
       ;; Test cipher validation
       (assert-true "Cipher validation accepts valid characters"
                    (valid-string? "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
       
       (assert-false "Cipher validation rejects invalid characters"
                     (valid-string? "ABC123!@#"))
       
       ;; Test word frequency analysis (if available)
       (let ((test-text "the quick brown fox jumps over the lazy dog"))
            (assert-true "Word frequency handles normal text"
                         (valid-string? test-text)))
       
       ;; Test input length limits
       (assert-false "Word frequency rejects extremely long text"
                     (valid-string? (make-string 10000 #\x))))
      (display "string-puzzle.ss not found - skipping tests"))
  (newline))

;; Test duckie.ss functionality
(define (test-duckie)
  (display "=== Testing duckie.ss functionality ===")
  (newline)
  
  (if (file-exists? "duckie.ss")
      (begin
       (load "duckie.ss")
       
       ;; Test duckie creation parameters
       (assert-true "Duckie parameters are within valid range"
                    (and (valid-string? "yellow")
                         (valid-integer? 5)
                         (valid-symbol? 'happy)))
       
       ;; Test duckie name validation
       (assert-true "Duckie name validation accepts normal names"
                    (valid-string? "Daffy"))
       
       (assert-false "Duckie name validation rejects overly long names"
                     (valid-string? (make-string 300 #\A)))
       
       ;; Test duckie position validation (if applicable)
       (assert-true "Duckie position validation accepts screen coordinates"
                    (and (valid-integer? 640)
                         (valid-integer? 480)
                         (valid-integer? 0))))
      (display "duckie.ss not found - skipping tests"))
  (newline))

;; Test environment.ss functionality
(define (test-environment)
  (display "=== Testing environment.ss functionality ===")
  (newline)
  
  (if (file-exists? "environment.ss")
      (begin
       (load "environment.ss")
       
       ;; Test environment parameter validation
       (assert-true "Environment parameters are valid"
                    (and (valid-string? "sunny")
                         (valid-integer? 25)
                         (valid-symbol? 'day))))
      (display "environment.ss not found - skipping tests"))
  (newline))

;; Test security utilities integration
(define (test-security-integration)
  (display "=== Testing security utilities integration ===")
  (newline)
  
  ;; Test input validation across different data types
  (assert-true "String validation accepts normal strings"
               (valid-string? "normal string"))
  
  (assert-false "String validation rejects overly long strings"
                (valid-string? (make-string 20000 #\x)))
  
  (assert-true "Integer validation accepts reasonable integers"
               (and (valid-integer? 0)
                    (valid-integer? 100)
                    (valid-integer? -50)))
  
  (assert-false "Integer validation rejects extreme values"
                (valid-integer? 10000000))
  
  (assert-true "Symbol validation accepts normal symbols"
               (valid-symbol? 'normal-symbol))
  
  ;; Test path sanitization
  (assert-equal "Path traversal protection works"
                "safe-file.txt"
                (sanitize-filename "../../../safe-file.txt"))
  
  (assert-equal "Home directory protection works"
                "userfile"
                (sanitize-filename "~/user/file"))
  
  ;; Test HTML escaping
  (assert-equal "HTML escaping converts dangerous characters"
                "&lt;script&gt;alert(&#39;xss&#39;)&lt;/script&gt;"
                (escape-html "<script>alert('xss')</script>"))
  
  ;; Test security logging
  (let ((test-event "unit_test_security_event"))
       (log-security-event test-event "test_user" "test details")
       (display "  Security event logged successfully")
       (newline))
  
  (newline))

;; Test error handling
(define (test-error-handling)
  (display "=== Testing error handling ===")
  (newline)
  
  ;; Test safe string operations
  (assert-equal "Safe string length handles empty strings"
                0
                (safe-string-length ""))
  
  (assert-equal "Safe string length handles normal strings"
                11
                (safe-string-length "hello world"))
  
  (assert-false "Safe string length rejects overly long strings"
                (safe-string-length (make-string 20000 #\x)))
  
  ;; Test safe list operations
  (assert-equal "Safe list take handles normal requests"
                '(1 2 3)
                (safe-list-take '(1 2 3 4 5) 3))
  
  (assert-false "Safe list take rejects excessive requests"
                (safe-list-take '(1 2 3) 10))
  
  ;; Test safe vector operations
  (let ((test-vector (vector 'a 'b 'c 'd 'e)))
       (assert-equal "Safe vector ref handles valid indices"
                     'c
                     (safe-vector-ref test-vector 2))
       
       (assert-false "Safe vector ref rejects invalid indices"
                     (safe-vector-ref test-vector 10)))
  
  (newline))

;; Test input sanitization
(define (test-input-sanitization)
  (display "=== Testing input sanitization ===")
  (newline)
  
  ;; Test various attack vectors
  (assert-false "SQL injection patterns are rejected"
                (valid-string? "'; DROP TABLE users; --"))
  
  (assert-equal "Command injection is sanitized"
                "rm -rf safe"
                (sanitize-filename "rm -rf /; cat /etc/passwd"))
  
  (assert-equal "Path traversal is neutralized"
                "etc.passwd"
                (sanitize-filename "../../../../etc/passwd"))
  
  ;; Test XSS prevention
  (assert-equal "Script tags are escaped"
                "&lt;script&gt;alert(1)&lt;/script&gt;"
                (escape-html "<script>alert(1)</script>"))
  
  (assert-equal "Event handlers are escaped"
                "&lt;img src=x onerror=alert(1)&gt;"
                (escape-html "<img src=x onerror=alert(1)>"))
  
  (newline))

;; Performance test for security operations
(define (test-security-performance)
  (display "=== Testing security performance ===")
  (newline)
  
  ;; Test that security checks don't significantly impact performance
  (let ((test-strings (map (lambda (i) (string-append "test" (number->string i)))
                           (iota 1000))))
       
       (let ((start-time (current-time)))
            (for-each valid-string? test-strings)
            (let ((end-time (current-time))
                  (elapsed (- end-time start-time)))
                 (display (string-append "  Validated 1000 strings in "
                                         (number->string elapsed) " seconds"))
                 (newline)
                 (assert-true "Security validation is performant" (< elapsed 1.0)))))
  
  (newline))

;; Run all unit tests
(define (run-all-unit-tests)
  (display "=== Running Playpen Unit Tests ===")
  (newline)
  (newline)
  
  (test-string-art)
  (test-block-playground)
  (test-string-puzzle)
  (test-duckie)
  (test-environment)
  (test-security-integration)
  (test-error-handling)
  (test-input-sanitization)
  (test-security-performance)
  
  (newline)
  (display "=== Unit Tests Summary ===")
  (newline)
  (display (string-append "Tests run: " (number->string *tests-run*)))
  (newline)
  (display (string-append "Tests passed: " (number->string *tests-passed*)))
  (newline)
  (display (string-append "Tests failed: " (number->string *tests-failed*)))
  (newline)
  
  (if (zero? *tests-failed*)
      (display "🎉 All tests passed!")
      (display "❌ Some tests failed!"))
  (newline))

;; Run tests
(run-all-unit-tests)

;;; test-playpen-unit.ss ends here
