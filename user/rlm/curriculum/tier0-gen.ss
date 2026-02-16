#!/usr/bin/env scheme-script
;;; tier0-gen.ss — Generate Tier 0 Scheme fluency training data as JSONL
;;;
;;; Targets the actual failure modes observed in RLM benchmark traces:
;;; - Python-style [0] indexing instead of (first ...) or (car ...)
;;; - string-split returns a list, not a string
;;; - Wrong argument counts/orders for string functions
;;; - Lambda syntax errors
;;; - let/let* binding form errors
;;; - Confusing driver actions with worker functions
;;;
;;; Output: user/rlm/curriculum/tier0-data.jsonl
;;; Format: {"prompt": "...", "ground_truth": "...", "category": "...", "alternatives": [...]}
;;;
;;; Each entry has one canonical ground_truth and zero or more acceptable alternatives.
;;; The reward function should accept any of them.

;; ---- Minimal JSON emitter (no external deps) ----

(define (json-escape s)
  (let loop ([i 0] [acc '()])
    (if (>= i (string-length s))
        (list->string (reverse acc))
        (let ([c (string-ref s i)])
          (cond
            [(char=? c #\") (loop (+ i 1) (append '(#\" #\\) acc))]
            [(char=? c #\\) (loop (+ i 1) (append '(#\\ #\\) acc))]
            [(char=? c #\newline) (loop (+ i 1) (append '(#\n #\\) acc))]
            [(char=? c #\tab) (loop (+ i 1) (append '(#\t #\\) acc))]
            [else (loop (+ i 1) (cons c acc))])))))

(define (emit-jsonl port prompt ground-truth category . alternatives)
  (let ([alts (if (null? alternatives) '() (car alternatives))])
    (display "{" port)
    (display (format "\"prompt\": \"~a\", " (json-escape prompt)) port)
    (display (format "\"ground_truth\": \"~a\", " (json-escape ground-truth)) port)
    (display (format "\"category\": \"~a\"" (json-escape category)) port)
    (when (not (null? alts))
      (display ", \"alternatives\": [" port)
      (let loop ([a alts] [first? #t])
        (unless (null? a)
          (unless first? (display ", " port))
          (display (format "\"~a\"" (json-escape (car a))) port)
          (loop (cdr a) #f)))
      (display "]" port))
    (display "}\n" port)))

;; ---- Prompt template ----

(define *system-context*
  (string-append
    "You are writing Scheme code for the Fold RLM agent. "
    "Available functions: first, second, third, fourth, fifth, rest, last, "
    "car, cdr, cadr, caddr, length, append, reverse, map, filter, for-each, "
    "string-split, string-trim, string-contains?, string-prefix?, string-suffix?, "
    "extract-after, split-lines, flatten, deduplicate, sorted, take, drop, "
    "cartesian-product, string-replace, string-join, string-index-of, substr, "
    "filter-map, iota, build-list, let, let*, lambda, define.\n\n"
    "IMPORTANT: This is Scheme, not Python. "
    "Use (first lst) not lst[0]. "
    "string-split returns a LIST of strings. "
    "filter and map require a lambda argument."))

(define (make-prompt task)
  (string-append *system-context* "\n\nTask: " task "\n\nWrite a single Scheme expression:"))

;; ---- Template Categories ----

;; Category 1: List Access
;; Targets: Python [0] indexing, wrong car/cdr nesting

(define *list-access-tasks*
  (list
    ;; first/second/third with literal lists
    (list "Extract the first element from the list '(apple banana cherry)"
          "(first '(apple banana cherry))"
          '("(car '(apple banana cherry))"))
    (list "Get the second element from the list '(10 20 30 40 50)"
          "(second '(10 20 30 40 50))"
          '("(cadr '(10 20 30 40 50))"))
    (list "Get the third element from '(red green blue yellow)"
          "(third '(red green blue yellow))"
          '("(caddr '(red green blue yellow))"))
    (list "Get the last element from the list '(a b c d e)"
          "(last '(a b c d e))"
          '())
    (list "Get all elements except the first from '(1 2 3 4 5)"
          "(rest '(1 2 3 4 5))"
          '("(cdr '(1 2 3 4 5))"))
    ;; Nested access
    (list "Get the first element of the list stored in variable data, where data is '(hello world foo)"
          "(first data)"
          '("(car data)"))
    (list "Get the second element from the result of (string-split \"a|b|c\" \"|\")"
          "(second (string-split \"a|b|c\" \"|\"))"
          '("(cadr (string-split \"a|b|c\" \"|\"))"))
    (list "Get the third element from the result of (string-split \"x:y:z:w\" \":\")"
          "(third (string-split \"x:y:z:w\" \":\"))"
          '("(caddr (string-split \"x:y:z:w\" \":\"))"))
    ;; Length
    (list "Count the number of elements in the list '(a b c d e f)"
          "(length '(a b c d e f))"
          '())
    (list "Count how many elements are in the list stored in variable items"
          "(length items)"
          '())
    ;; take/drop
    (list "Get the first 3 elements from the list '(1 2 3 4 5 6 7)"
          "(take '(1 2 3 4 5 6 7) 3)"
          '())
    (list "Remove the first 2 elements from '(a b c d e)"
          "(drop '(a b c d e) 2)"
          '())
    ;; reverse
    (list "Reverse the list '(1 2 3 4 5)"
          "(reverse '(1 2 3 4 5))"
          '())
    ;; append
    (list "Concatenate the lists '(1 2 3) and '(4 5 6)"
          "(append '(1 2 3) '(4 5 6))"
          '())))

;; Category 2: String Operations
;; Targets: string-split returning list (not string), arg order for prefix?/suffix?

(define *string-ops-tasks*
  (list
    ;; string-split basics
    (list "Split the string \"hello world foo\" by spaces"
          "(string-split \"hello world foo\" \" \")"
          '())
    (list "Split \"a|b|c|d\" by the pipe character"
          "(string-split \"a|b|c|d\" \"|\")"
          '())
    (list "Split \"name: Alice | age: 30 | city: NYC\" by \" | \" and get the second part"
          "(second (string-split \"name: Alice | age: 30 | city: NYC\" \" | \"))"
          '("(cadr (string-split \"name: Alice | age: 30 | city: NYC\" \" | \"))"))
    (list "Split \"user: U007 | date: 2025-03-15\" by \" | \" and get the first part"
          "(first (string-split \"user: U007 | date: 2025-03-15\" \" | \"))"
          '("(car (string-split \"user: U007 | date: 2025-03-15\" \" | \"))"))
    ;; string-contains?
    (list "Check if the string \"hello world\" contains \"world\""
          "(string-contains? \"hello world\" \"world\")"
          '())
    (list "Check if the string stored in variable line contains \"ENTRY\""
          "(string-contains? line \"ENTRY\")"
          '())
    ;; string-prefix?
    (list "Check if the string \"ENTRY | user: U001\" starts with \"ENTRY\""
          "(string-prefix? \"ENTRY\" \"ENTRY | user: U001\")"
          '())
    (list "Check if variable line starts with \"HEADER\""
          "(string-prefix? \"HEADER\" line)"
          '())
    ;; string-suffix?
    (list "Check if \"report.pdf\" ends with \".pdf\""
          "(string-suffix? \".pdf\" \"report.pdf\")"
          '())
    ;; string-trim
    (list "Remove leading and trailing whitespace from \"  hello  \""
          "(string-trim \"  hello  \")"
          '())
    ;; extract-after
    (list "Extract everything after \"user: \" in the string \"ENTRY | user: U007 | date: 2025-03-15\""
          "(extract-after \"ENTRY | user: U007 | date: 2025-03-15\" \"user: \")"
          '())
    ;; string-join
    (list "Join the list '(\"a\" \"b\" \"c\") with \", \" as separator"
          "(string-join '(\"a\" \"b\" \"c\") \", \")"
          '())
    ;; string-replace
    (list "Replace all occurrences of \"old\" with \"new\" in \"old value is old\""
          "(string-replace \"old value is old\" \"old\" \"new\")"
          '())
    ;; substr
    (list "Extract characters 0 through 4 from \"hello world\""
          "(substr \"hello world\" 0 5)"
          '("(substring \"hello world\" 0 5)"))
    ;; string-index-of
    (list "Find the position of \"world\" in \"hello world\""
          "(string-index-of \"hello world\" \"world\")"
          '())
    ;; Composed string operations
    (list "Split \"ENTRY | user: U007 | date: 2025-03-15 | category: science\" by \" | \", get the second part, then extract after \"user: \""
          "(extract-after (second (string-split \"ENTRY | user: U007 | date: 2025-03-15 | category: science\" \" | \")) \"user: \")"
          '())
    (list "Split \"category: science\" by \": \" and get the second element"
          "(second (string-split \"category: science\" \": \"))"
          '("(cadr (string-split \"category: science\" \": \"))"))))

;; Category 3: Filter and Map with Lambdas
;; Targets: missing lambda, wrong lambda syntax, Python list comprehension style

(define *filter-map-tasks*
  (list
    ;; Basic filter
    (list "Filter the list '(1 2 3 4 5 6 7 8) to keep only even numbers"
          "(filter (lambda (n) (= (remainder n 2) 0)) '(1 2 3 4 5 6 7 8))"
          '("(filter even? '(1 2 3 4 5 6 7 8))"))
    (list "Filter a list of strings stored in variable lines to keep only those containing \"ENTRY\""
          "(filter (lambda (line) (string-contains? line \"ENTRY\")) lines)"
          '())
    (list "Filter entries to keep only those where the string contains \"science\""
          "(filter (lambda (entry) (string-contains? entry \"science\")) entries)"
          '())
    (list "Filter a list of numbers stored in variable nums to keep only those greater than 10"
          "(filter (lambda (n) (> n 10)) nums)"
          '())
    ;; Basic map
    (list "Double every number in the list '(1 2 3 4 5)"
          "(map (lambda (n) (* n 2)) '(1 2 3 4 5))"
          '())
    (list "Extract the first element from each sublist in '((a 1) (b 2) (c 3))"
          "(map first '((a 1) (b 2) (c 3)))"
          '("(map car '((a 1) (b 2) (c 3)))"))
    (list "Convert a list of strings '(\"hello\" \"world\") to their lengths"
          "(map string-length '(\"hello\" \"world\"))"
          '())
    (list "Trim whitespace from each string in the list stored in variable raw-lines"
          "(map string-trim raw-lines)"
          '())
    ;; filter-map
    (list "From a list of strings in variable lines, extract the part after \"user: \" for lines that contain \"user: \", discarding lines without it"
          "(filter-map (lambda (line) (and (string-contains? line \"user: \") (extract-after line \"user: \"))) lines)"
          '())
    ;; map then filter
    (list "From entries, extract the category (split by \" | \" and take last element), then filter for \"science\""
          "(filter (lambda (cat) (string=? cat \"science\")) (map (lambda (e) (last (string-split e \" | \"))) entries))"
          '())
    ;; Nested map
    (list "Given a list of entry strings in variable entries, extract the user ID from each (split by \" | \", take second part, extract after \"user: \")"
          "(map (lambda (e) (extract-after (second (string-split e \" | \")) \"user: \")) entries)"
          '())
    ;; deduplicate after map
    (list "Get unique user IDs from a list of user IDs stored in variable user-ids"
          "(deduplicate user-ids)"
          '("(remove-duplicates user-ids)"))))

;; Category 4: Let Bindings
;; Targets: wrong let syntax, missing parens around bindings

(define *let-binding-tasks*
  (list
    (list "Use let to bind x to 10 and y to 20, then return their sum"
          "(let ([x 10] [y 20]) (+ x y))"
          '("(let ((x 10) (y 20)) (+ x y))"))
    (list "Use let* to split \"a|b|c\" by \"|\" and bind the result to parts, then get the second element"
          "(let* ([parts (string-split \"a|b|c\" \"|\")]) (second parts))"
          '("(let* ((parts (string-split \"a|b|c\" \"|\"))) (second parts))"))
    (list "Use let* to: split the entry string by \" | \", extract the user part (second element), then extract after \"user: \""
          "(let* ([parts (string-split entry \" | \")] [user-part (second parts)] [user-id (extract-after user-part \"user: \")]) user-id)"
          '())
    (list "Use let to bind data to '(1 2 3 4 5) and compute its length"
          "(let ([data '(1 2 3 4 5)]) (length data))"
          '("(let ((data '(1 2 3 4 5))) (length data))"))
    (list "Use let* to bind parts to (string-split line \" | \"), category to (last parts), and date to (third parts), then return a list of category and date"
          "(let* ([parts (string-split line \" | \")] [category (last parts)] [date (third parts)]) (list category date))"
          '())))

;; Category 5: Sorting and Aggregation
;; Targets: sorted without predicate, wrong sort function names

(define *sort-aggregate-tasks*
  (list
    (list "Sort the list '(3 1 4 1 5 9) in ascending order"
          "(sorted '(3 1 4 1 5 9) <)"
          '())
    (list "Sort a list of strings '(\"banana\" \"apple\" \"cherry\") alphabetically"
          "(sorted '(\"banana\" \"apple\" \"cherry\") string<?)"
          '())
    (list "Sort user IDs stored in variable ids alphabetically"
          "(sorted ids string<?)"
          '())
    (list "Remove duplicates from the list '(1 2 2 3 3 3 4)"
          "(deduplicate '(1 2 2 3 3 3 4))"
          '("(remove-duplicates '(1 2 2 3 3 3 4))"))
    (list "Sort the list '(5 2 8 1 9) in descending order"
          "(sorted '(5 2 8 1 9) >)"
          '())
    (list "Get unique elements from a list stored in variable results, then sort them"
          "(sorted (deduplicate results) string<?)"
          '())))

;; Category 6: Cartesian Product and Pairs
;; Targets: this is exactly what the Pairs benchmark needs

(define *pairs-tasks*
  (list
    (list "Generate all pairs from set-a = '(\"U001\" \"U003\") and set-b = '(\"U002\" \"U004\")"
          "(cartesian-product '(\"U001\" \"U003\") '(\"U002\" \"U004\"))"
          '())
    (list "Generate all pairs from lists stored in variables science-users and tech-users"
          "(cartesian-product science-users tech-users)"
          '())
    (list "Generate all pairs and sort them"
          "(sorted (cartesian-product set-a set-b) list<?)"
          '())))

;; Category 7: Composition / Multi-Step
;; Targets: real benchmark patterns, end-to-end fluency

(define *composition-tasks*
  (list
    (list "From a list of entry strings in variable entries, filter those containing \"science\", extract user IDs (split by \" | \", second element, extract after \"user: \"), and deduplicate"
          "(deduplicate (map (lambda (e) (extract-after (second (string-split e \" | \")) \"user: \")) (filter (lambda (e) (string-contains? e \"science\")) entries)))"
          '())
    (list "Count how many entries in variable entries contain the word \"technology\""
          "(length (filter (lambda (e) (string-contains? e \"technology\")) entries))"
          '())
    (list "From entries, filter those with \"category: science\", then check if any have a date before \"2025-06-01\" by extracting the date field"
          "(filter (lambda (e) (let* ([parts (string-split e \" | \")] [date-part (third parts)] [date (extract-after date-part \"date: \")]) (string<? date \"2025-06-01\"))) (filter (lambda (e) (string-contains? e \"category: science\")) entries))"
          '())
    (list "Flatten a list of lists stored in variable chunks, then get the total count"
          "(length (flatten chunks))"
          '())
    (list "Given map-result (a list of lists from map-chunks), flatten it, filter for lines containing \"ENTRY\", and count them"
          "(length (filter (lambda (line) (string-contains? line \"ENTRY\")) (flatten map-result)))"
          '())
    (list "Build a list of 5 elements where each element is the square of its index (0-4)"
          "(build-list 5 (lambda (i) (* i i)))"
          '())
    (list "Generate a range from 0 to 9"
          "(iota 10)"
          '())
    (list "From a list of entry strings, extract (user-id, category) pairs using let* to parse each entry"
          "(map (lambda (e) (let* ([parts (string-split e \" | \")] [user-id (extract-after (second parts) \"user: \")] [category (extract-after (last parts) \"category: \")]) (list user-id category))) entries)"
          '())
    (list "Split lines of text stored in variable text into individual lines, filter for non-empty ones"
          "(filter (lambda (line) (> (string-length line) 0)) (split-lines text))"
          '())
    (list "Join a sorted, deduplicated list of user IDs with newlines"
          "(string-join (sorted (deduplicate user-ids) string<?) \"\\n\")"
          '())))

;; Category 8: Common Anti-Patterns (what NOT to do)
;; These have prompts that tempt Python/JS idioms, correct Scheme answers

(define *anti-pattern-tasks*
  (list
    (list "Get the element at index 0 from the list '(a b c d)"
          "(first '(a b c d))"
          '("(car '(a b c d))"))
    (list "Get the element at index 2 from '(10 20 30 40 50)"
          "(third '(10 20 30 40 50))"
          '("(caddr '(10 20 30 40 50))"))
    (list "Access the last element (index -1) of '(x y z)"
          "(last '(x y z))"
          '())
    (list "Slice elements from index 1 to 3 from '(a b c d e)"
          "(take (drop '(a b c d e) 1) 2)"
          '())
    (list "Check if a string starts with a prefix — the string is \"ENTRY | data\" and the prefix is \"ENTRY\""
          "(string-prefix? \"ENTRY\" \"ENTRY | data\")"
          '())
    (list "Split \"a,b,c\" by comma and get the length of the resulting list"
          "(length (string-split \"a,b,c\" \",\"))"
          '())
    (list "Given a list of strings in variable data, get a new list containing only the first 3 strings"
          "(take data 3)"
          '())))

;; ---- Generate ----

(define (generate-all output-file)
  (let ([all-tasks (list
                     (cons "list-access" *list-access-tasks*)
                     (cons "string-ops" *string-ops-tasks*)
                     (cons "filter-map" *filter-map-tasks*)
                     (cons "let-bindings" *let-binding-tasks*)
                     (cons "sort-aggregate" *sort-aggregate-tasks*)
                     (cons "pairs" *pairs-tasks*)
                     (cons "composition" *composition-tasks*)
                     (cons "anti-patterns" *anti-pattern-tasks*))])
    (call-with-output-file output-file
      (lambda (port)
        (let ([count 0])
          (for-each
            (lambda (category-pair)
              (let ([category (car category-pair)]
                    [tasks (cdr category-pair)])
                (for-each
                  (lambda (task)
                    (let ([prompt (make-prompt (car task))]
                          [ground-truth (cadr task)]
                          [alts (caddr task)])
                      (emit-jsonl port prompt ground-truth category alts)
                      (set! count (+ count 1))))
                  tasks)))
            all-tasks)
          (display (format "Generated ~a tasks to ~a\n" count output-file))
          (display "Categories:\n")
          (for-each
            (lambda (cp)
              (display (format "  ~a: ~a tasks\n" (car cp) (length (cdr cp)))))
            all-tasks))))))

(generate-all "user/rlm/curriculum/tier0-data.jsonl")
