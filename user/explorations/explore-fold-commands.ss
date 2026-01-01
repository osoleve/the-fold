;;; explore-fold-commands.ss — Test Fold-specific commands and features

(load "shell/repl.ss")

(display "\n╔════════════════════════════════════════════════════════════╗\n")
(display "║        THE FOLD — COMMAND SYSTEM EXPLORATION              ║\n")
(display "╚════════════════════════════════════════════════════════════╝\n\n")

;;; Test 1: Commands discovery
(display "TEST 1: Available Commands\n")
(display "  (commands) => \n")
(guard (e (else (display "    [ERROR] ")))
       (commands))
(display "\n")

;;; Test 2: Help system
(display "\nTEST 2: Help System\n")
(display "  (help) => \n")
(guard (e (else (display "    [ERROR] ")))
       (help))
(display "\n")

;;; Test 3: Version command
(display "\nTEST 3: Version Command\n")
(display "  (version) => ")
(guard (e (else (display "[ERROR]")))
       (version))
(display "\n")

;;; Test 4: Session info
(display "\nTEST 4: Who Command\n")
(display "  (who) => ")
(guard (e (else (display "[ERROR]")))
       (who))
(display "\n")

;;; Test 5: Chat command
(display "\nTEST 5: Chat Command\n")
(display "  (chat \"test message\") => ")
(guard (e (else (display "[ERROR or NOT AVAILABLE]")))
       (display (chat "test message")))
(display "\n")

;;; Test 6: String utilities if loaded
(display "\nTEST 6: String Utilities\n")
(display "  (string-pad \"hello\" 10) => ")
(guard (e (else (display "[NOT AVAILABLE]")))
       (display (string-pad "hello" 10)))
(display "\n")

;;; Test 7: Multi-line strings
(display "\nTEST 7: Multi-line String Literals\n")
(define multiline "line 1
line 2
line 3")
(display "  Multi-line string: ")
(display multiline)
(display "\n")

;;; Test 8: Syntax errors recovery
(display "\nTEST 8: Syntax Error Handling\n")
(display "  Testing invalid syntax: (+ 1 2\n")
(guard (e (else
           (display "    [CAUGHT ERROR] ")
           (display (condition-message e))
           (display "\n")))
       (eval (read)))
(display "\n")

;;; Test 9: Define function and use it
(display "\nTEST 9: Define and Call Function\n")
(define (greet name)
  (string-append "Hello, " name "!"))
(display "  (greet \"Fold\") => ")
(display (greet "Fold"))
(display "\n")

;;; Test 10: Define macro-like patterns
(display "\nTEST 10: Quoted Lists and Processing\n")
(define data '((name "Alice") (age 30) (city "NYC")))
(display "  Processing alist: ")
(guard (e (else (display "[ERROR]")))
       (display (assoc 'name data)))
(display "\n")

;;; Test 11: Tail recursion
(display "\nTEST 11: Tail-recursive Function\n")
(define (sum-list lst acc)
  (if (null? lst)
      acc
      (sum-list (cdr lst) (+ acc (car lst)))))
(display "  (sum-list '(1 2 3 4 5) 0) => ")
(display (sum-list '(1 2 3 4 5) 0))
(display "\n")

;;; Test 12: Let* binding
(display "\nTEST 12: Let* Bindings\n")
(display "  (let* ((x 1) (y (+ x 1))) (+ x y)) => ")
(guard (e (else (display "[ERROR]")))
       (display (let* ((x 1) (y (+ x 1))) (+ x y))))
(display "\n")

;;; Test 13: Lambda with multiple args
(display "\nTEST 13: Lambda with Variable Arguments\n")
(define (sum-args . args)
  (foldr + 0 args))
(display "  Attempting (sum-args 1 2 3 4)...\n")
(guard (e (else
           (display "    [ERROR - foldr may not be available] ")))
       (display "    Result: ")
       (display (sum-args 1 2 3 4))
       (display "\n"))

;;; Test 14: Named let
(display "\nTEST 14: Named Let (Loop)\n")
(display "  Testing named let for countdown...\n")
(guard (e (else (display "    [ERROR]")))
       (let countdown ((n 3))
            (display "    ")
            (display n)
            (if (> n 1)
                (countdown (- n 1))
                (display " done!")))
       (display "\n"))

;;; Test 15: String utilities - split/join
(display "\nTEST 15: String Manipulation\n")
(display "  (string-split \"a,b,c\" \",\") => ")
(guard (e (else (display "[NOT AVAILABLE]")))
       (display (string-split "a,b,c" ",")))
(display "\n")

;;; Test 16: Map with multiple lists
(display "\nTEST 16: Map with Multiple Lists\n")
(display "  (map + '(1 2 3) '(10 20 30)) => ")
(guard (e (else (display "[ERROR]")))
       (display (map + '(1 2 3) '(10 20 30))))
(display "\n")

;;; Test 17: For-each
(display "\nTEST 17: For-each\n")
(display "  (for-each (lambda (x) (display x) (display \" \")) '(a b c))\n")
(display "  Output: ")
(guard (e (else (display "[ERROR]")))
       (for-each (lambda (x) (display x) (display " ")) '(a b c)))
(display "\n")

;;; Test 18: Partition lists
(display "\nTEST 18: List Partition\n")
(display "  Testing partition of '(1 2 3 4 5) into odds/evens...\n")
(let ((nums '(1 2 3 4 5)))
     (define odds (filter (lambda (x) (= 1 (modulo x 2))) nums))
     (define evens (filter (lambda (x) (= 0 (modulo x 2))) nums))
     (display "    Odds: ")
     (display odds)
     (display "\n    Evens: ")
     (display evens)
     (display "\n"))

;;; Test 19: Nested map
(display "\nTEST 19: Nested Map\n")
(display "  (map (lambda (x) (map (lambda (y) (* x y)) '(1 2 3))) '(1 2 3)) => \n    ")
(guard (e (else (display "[ERROR]")))
       (display (map (lambda (x) (map (lambda (y) (* x y)) '(1 2 3))) '(1 2 3))))
(display "\n")

;;; Test 20: Try using common list predicates
(display "\nTEST 20: List Predicates\n")
(display "  (member 2 '(1 2 3)) => ")
(guard (e (else (display "[NOT AVAILABLE]")))
       (display (member 2 '(1 2 3))))
(display "\n")

(display "\n╔════════════════════════════════════════════════════════════╗\n")
(display "║          FOLD COMMANDS EXPLORATION COMPLETE                ║\n")
(display "╚════════════════════════════════════════════════════════════╝\n")
