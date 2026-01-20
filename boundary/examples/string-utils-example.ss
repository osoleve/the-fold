(load "boundary/tools/string-utils.ss")

(doc 'module 'string-utils-example)
(doc 'description "Examples of String Utilities Usage - Demonstrates the canonical string utilities in boundary/string-utils.ss")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(printf "\n=== String Utilities Examples ===\n\n")

(doc 'section 'example-1-parsing-csv)

(printf "Example 1: Parsing CSV Data\n")
(printf "----\n")

(define csv-line "Alice,30,Engineer")
(define fields (string-split csv-line #\,))

(printf "CSV: ~s\n" csv-line)
(printf "Fields: ~s\n" fields)
(printf "Name: ~a, Age: ~a, Role: ~a\n\n"
        (car fields)
        (cadr fields)
        (caddr fields))

(doc 'section 'example-2-log-lines)

(printf "Example 2: Processing Log Lines\n")
(printf "----\n")

(define log-text "[ERROR] Failed to connect\n[INFO] Retrying connection\n[ERROR] Timeout occurred")
(define log-lines (string-split-lines log-text))

(printf "Log text:\n~a\n\n" log-text)
(printf "Processing ~a log lines:\n" (length log-lines))

(for-each
 (lambda (line)
         (when (string-contains? line "ERROR")
               (printf "  ! ~a\n" line)))
 log-lines)

(printf "\n")

(doc 'section 'example-3-building-paths)

(printf "Example 3: Building Paths\n")
(printf "----\n")

(define path-components '("home" "user" "documents" "file.txt"))
(define unix-path (string-join path-components "/"))
(define windows-path (string-join path-components "\\"))

(printf "Components: ~s\n" path-components)
(printf "Unix path: ~a\n" unix-path)
(printf "Windows path: ~a\n\n" windows-path)

;;; ====
;;; Example 4: Cleaning User Input
;;; ====

(printf "Example 4: Cleaning User Input\n")
(printf "----\n")

(define messy-input "  \n\t  Hello World!  \r\n  ")
(define clean-input (string-trim messy-input))

(printf "Raw input: ~s\n" messy-input)
(printf "Cleaned: ~s\n" clean-input)
(printf "Is blank? ~a\n\n" (string-blank? messy-input))

;;; ====
;;; Example 5: Template Replacement
;;; ====

(printf "Example 5: Template Replacement\n")
(printf "----\n")

(define template "Hello {{name}}, welcome to {{place}}!")
(define personalized
  (string-replace
   (string-replace template "{{name}}" "Alice")
   "{{place}}" "The Fold"))

(printf "Template: ~a\n" template)
(printf "Result: ~a\n\n" personalized)

;;; ====
;;; Example 6: File Extension Checking
;;; ====

(printf "Example 6: File Extension Checking\n")
(printf "----\n")

(define files '("script.ss" "README.md" "data.json" "test.txt"))

(printf "Scheme files:\n")
(for-each
 (lambda (file)
         (when (string-ends-with? file ".ss")
               (printf "  • ~a\n" file)))
 files)

(printf "\nMarkdown files:\n")
(for-each
 (lambda (file)
         (when (string-ends-with? file ".md")
               (printf "  • ~a\n" file)))
 files)

(printf "\n")

;;; ====
;;; Example 7: Finding Keywords
;;; ====

(printf "Example 7: Finding Keywords\n")
(printf "----\n")

(define code-snippet "(define (factorial n) ...)")
(define has-define? (string-contains? code-snippet "define"))
(define has-lambda? (string-contains? code-snippet "lambda"))

(printf "Code: ~a\n" code-snippet)
(printf "Contains 'define'? ~a\n" has-define?)
(printf "Contains 'lambda'? ~a\n\n" has-lambda?)

;;; ====
;;; Example 8: Unicode Support
;;; ====

(printf "Example 8: Unicode Support\n")
(printf "----\n")

(define unicode-text "Hello 世界 🌍")
(define parts (string-split unicode-text #\space))

(printf "Text: ~a\n" unicode-text)
(printf "Split by space: ~s\n" parts)
(printf "Contains '世界'? ~a\n" (string-contains? unicode-text "世界"))
(printf "Starts with 'Hello'? ~a\n\n" (string-starts-with? unicode-text "Hello"))

;;; ====

(printf "=== All examples complete! ===\n\n")
(printf "See boundary/string-utils.ss for full API documentation.\n")
(printf "See boundary/test-string-utils.ss for comprehensive tests.\n\n")
