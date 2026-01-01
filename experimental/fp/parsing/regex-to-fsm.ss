;;; fabric/stitches/fp/regex-to-fsm.ss — Regex to Automata Conversion
;;;
;;; Thompson's NFA Construction Algorithm
;;;
;;; Converts regular expression AST (from regex.ss) into
;;; epsilon-NFA (for fsm.ss) using Thompson's construction.
;;;
;;; CURRENT LIMITATIONS:
;;;   - Character classes (rx-char-class, rx-dot, rx-one-of) have partial support
;;;   - Core regex operators (char, seq, alt, star, plus, opt) fully supported
;;;   - 36/39 tests passing (character class tests fail)
;;;   - Future work: Extend fsm.ss to handle char-class transitions or
;;;     expand character classes to explicit alternations during construction
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/regex.ss
;;;   - fp/fsm.ss

(load "core/prelude.ss")
(load "core/fp/parsing/regex.ss")
(load "core/fp/parsing/fsm.ss")

;;; ============================================================
;;; Thompson's NFA Construction
;;; ============================================================
;;;
;;; Thompson's algorithm builds an NFA fragment for each regex construct,
;;; then combines them using epsilon transitions.
;;;
;;; Each fragment has:
;;;   - Exactly one start state
;;;   - Exactly one accepting state
;;;   - No transitions into start
;;;   - No transitions out of accept

;;; NFA Fragment = (fragment states alphabet transitions start accept epsilon)
;;; Temporary structure during construction.
(define (make-fragment states alphabet transitions start accept epsilon)
  (list 'fragment states alphabet transitions start accept epsilon))

(define (fragment-states f) (list-ref f 1))
(define (fragment-alphabet f) (list-ref f 2))
(define (fragment-transitions f) (list-ref f 3))
(define (fragment-start f) (list-ref f 4))
(define (fragment-accept f) (list-ref f 5))
(define (fragment-epsilon f) (list-ref f 6))

;;; Counter for generating unique state names
(define *thompson-counter* 0)
(define (thompson-fresh-state)
  (set! *thompson-counter* (+ *thompson-counter* 1))
  (string->symbol (string-append "t" (number->string *thompson-counter*))))

;;; Reset counter (useful for testing)
(define (thompson-reset-counter!)
  (set! *thompson-counter* 0))

;;; ============================================================
;;; Fragment Constructors
;;; ============================================================

;;; thompson-char : Char -> Fragment
;;; NFA fragment that matches a single character.
(define (thompson-char c)
  (let ([start (thompson-fresh-state)]
        [accept (thompson-fresh-state)])
       (make-fragment
        (list start accept)
        (list c)
        (list (cons (cons start c) (list accept)))
        start
        accept
        '())))

;;; thompson-char-class : (Char -> Boolean) -> String -> Fragment
;;; NFA fragment for character class (uses epsilon to branches for each char).
;;; For simplicity, we'll treat this as accepting any char and check in transitions.
;;; A more efficient approach would expand to all matching chars, but that's
;;; impractical for large classes. Instead, we use a special transition marker.
(define (thompson-char-class pred description)
  (let ([start (thompson-fresh-state)]
        [accept (thompson-fresh-state)])
       ;; Store the predicate as metadata - actual matching will be done
       ;; by extending fsm-delta to handle char-class transitions
       ;; For now, we'll use a symbol to represent this
       (make-fragment
        (list start accept)
        (list 'char-class)  ; Special marker
        (list (cons (cons start (cons 'char-class pred)) (list accept)))
        start
        accept
        '())))

;;; thompson-epsilon : Fragment
;;; NFA fragment that matches empty string only.
(define (thompson-epsilon)
  (let ([start (thompson-fresh-state)])
       (make-fragment
        (list start)
        '()
        '()
        start
        start  ; Start = accept
        '())))

;;; thompson-empty : Fragment
;;; NFA fragment that matches nothing.
(define (thompson-empty)
  (let ([start (thompson-fresh-state)]
        [accept (thompson-fresh-state)])
       (make-fragment
        (list start accept)
        '()
        '()
        start
        accept
        '())))

;;; thompson-alt : Fragment -> Fragment -> Fragment
;;; Alternation: matches f1 or f2.
(define (thompson-alt f1 f2)
  (let ([new-start (thompson-fresh-state)]
        [new-accept (thompson-fresh-state)])
       (make-fragment
        (cons new-start
              (cons new-accept
                    (append (fragment-states f1) (fragment-states f2))))
        (union equal? (fragment-alphabet f1) (fragment-alphabet f2))
        (append (fragment-transitions f1) (fragment-transitions f2))
        new-start
        new-accept
        ;; Epsilon from new-start to both f1 and f2 starts
        ;; Epsilon from both f1 and f2 accepts to new-accept
        (append (list (cons new-start (list (fragment-start f1) (fragment-start f2)))
                      (cons (fragment-accept f1) (list new-accept))
                      (cons (fragment-accept f2) (list new-accept)))
                (fragment-epsilon f1)
                (fragment-epsilon f2)))))

;;; thompson-seq : Fragment -> Fragment -> Fragment
;;; Sequence: matches f1 followed by f2.
(define (thompson-seq f1 f2)
  (make-fragment
   (append (fragment-states f1) (fragment-states f2))
   (union equal? (fragment-alphabet f1) (fragment-alphabet f2))
   (append (fragment-transitions f1) (fragment-transitions f2))
   (fragment-start f1)
   (fragment-accept f2)
   ;; Epsilon from f1's accept to f2's start
   (append (list (cons (fragment-accept f1) (list (fragment-start f2))))
           (fragment-epsilon f1)
           (fragment-epsilon f2))))

;;; thompson-star : Fragment -> Fragment
;;; Kleene star: matches f zero or more times.
(define (thompson-star f)
  (let ([new-start (thompson-fresh-state)]
        [new-accept (thompson-fresh-state)])
       (make-fragment
        (cons new-start (cons new-accept (fragment-states f)))
        (fragment-alphabet f)
        (fragment-transitions f)
        new-start
        new-accept
        ;; Epsilon from new-start to f's start and new-accept (zero repetitions)
        ;; Epsilon from f's accept back to f's start (loop) and to new-accept
        (append (list (cons new-start (list (fragment-start f) new-accept))
                      (cons (fragment-accept f) (list (fragment-start f) new-accept)))
                (fragment-epsilon f)))))

;;; ============================================================
;;; Regex to Fragment Conversion
;;; ============================================================

;;; rx->fragment : Regex -> Fragment
;;; Convert regex AST to NFA fragment using Thompson's construction.
(define (rx->fragment r)
  (cond
   [(rx-empty? r) (thompson-empty)]
   [(rx-epsilon? r) (thompson-epsilon)]
   [(rx-char? r) (thompson-char (rx-char-value r))]
   [(rx-char-class? r)
    (thompson-char-class (rx-char-class-pred r) (rx-char-class-desc r))]
   [(rx-seq? r)
    (thompson-seq (rx->fragment (rx-seq-first r))
                  (rx->fragment (rx-seq-second r)))]
   [(rx-alt? r)
    (thompson-alt (rx->fragment (rx-alt-left r))
                  (rx->fragment (rx-alt-right r)))]
   [(rx-star? r)
    (thompson-star (rx->fragment (rx-star-inner r)))]
   [else (thompson-empty)]))

;;; ============================================================
;;; Fragment to FSM Conversion
;;; ============================================================

;;; fragment->fsm : Fragment -> FSM
;;; Convert NFA fragment to complete FSM.
(define (fragment->fsm frag)
  (make-fsm
   (fragment-states frag)
   (fragment-alphabet frag)
   (fragment-transitions frag)
   (fragment-start frag)
   (list (fragment-accept frag))
   (fragment-epsilon frag)))

;;; ============================================================
;;; High-Level API
;;; ============================================================

;;; rx->nfa : Regex -> FSM
;;; Convert regex to epsilon-NFA using Thompson's construction.
(define (rx->nfa r)
  (thompson-reset-counter!)
  (fragment->fsm (rx->fragment r)))

;;; rx->dfa : Regex -> FSM
;;; Convert regex to DFA (via NFA and subset construction).
(define (rx->dfa r)
  (nfa->dfa (rx->nfa r)))

;;; rx->minimized-dfa : Regex -> FSM
;;; Convert regex to minimized DFA.
(define (rx->minimized-dfa r)
  (fsm-minimize (rx->dfa r)))

;;; ============================================================
;;; String to Regex Parsing (Simple)
;;; ============================================================
;;;
;;; Simple parser for basic regex syntax.
;;; Supports: char, ., *, +, ?, |, (), []

;;; parse-regex : String -> Regex
;;; Parse simple regex string to AST.
;;; This is a simplified parser - full implementation would need
;;; proper lexing/parsing with precedence and escaping.
(define (parse-regex s)
  (let ([chars (string->list s)])
       (car (parse-alt chars))))

;;; Parser returns (regex . remaining-chars)

(define (parse-alt chars)
  (let ([result (parse-seq chars)])
       (if (and (pair? (cdr result))
                (char=? (car (cdr result)) #\|))
           (let ([right (parse-alt (cdr (cdr result)))])
                (cons (rx-alt (car result) (car right)) (cdr right)))
           result)))

(define (parse-seq chars)
  (let loop ([acc rx-epsilon] [cs chars])
       (if (or (null? cs)
               (char=? (car cs) #\))
               (char=? (car cs) #\|))
           (cons acc cs)
           (let ([result (parse-postfix cs)])
                (loop (if (rx-epsilon? acc)
                          (car result)
                          (rx-seq acc (car result)))
                      (cdr result))))))

(define (parse-postfix chars)
  (let ([result (parse-primary chars)])
       (if (null? (cdr result))
           result
           (case (car (cdr result))
                 [(#\*)
                  (cons (rx-star (car result)) (cdr (cdr result)))]
                 [(#\+)
                  (cons (rx-plus (car result)) (cdr (cdr result)))]
                 [(#\?)
                  (cons (rx-opt (car result)) (cdr (cdr result)))]
                 [else result]))))

(define (parse-primary chars)
  (cond
   [(null? chars) (cons rx-epsilon '())]
   [(char=? (car chars) #\.)
    (cons rx-dot (cdr chars))]
   [(char=? (car chars) #\()
    (let ([result (parse-alt (cdr chars))])
         (if (and (pair? (cdr result))
                  (char=? (car (cdr result)) #\)))
             (cons (car result) (cdr (cdr result)))
             result))]
   [(char=? (car chars) #\[)
    (parse-char-class (cdr chars))]
   [(char=? (car chars) #\\)
    (parse-escape (cdr chars))]
   [else
    (cons (rx-char (car chars)) (cdr chars))]))

(define (parse-char-class chars)
  ;; Simple [abc] or [^abc] parsing
  (let ([negated (and (pair? chars) (char=? (car chars) #\^))])
       (let loop ([cs (if negated (cdr chars) chars)] [acc ""])
            (cond
             [(null? cs) (cons rx-empty '())]
             [(char=? (car cs) #\])
              (cons (if negated
                        (rx-none-of acc)
                        (rx-one-of acc))
                    (cdr cs))]
             [else (loop (cdr cs) (string-append acc (string (car cs))))]))))

(define (parse-escape chars)
  (cond
   [(null? chars) (cons rx-empty '())]
   [(char=? (car chars) #\d) (cons rx-digit (cdr chars))]
   [(char=? (car chars) #\D) (cons rx-non-digit (cdr chars))]
   [(char=? (car chars) #\w) (cons rx-word (cdr chars))]
   [(char=? (car chars) #\W) (cons rx-non-word (cdr chars))]
   [(char=? (car chars) #\s) (cons rx-space (cdr chars))]
   [(char=? (car chars) #\S) (cons rx-non-space (cdr chars))]
   [else (cons (rx-char (car chars)) (cdr chars))]))

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

;;; regex-match? : String -> String -> Boolean
;;; Parse regex string and test if it matches input string.
(define (regex-match? pattern input)
  (fsm-accepts? (rx->nfa (parse-regex pattern)) input))

;;; regex-compile : String -> FSM
;;; Compile regex string to optimized DFA.
(define (regex-compile pattern)
  (rx->minimized-dfa (parse-regex pattern)))

;;; ============================================================
;;; Exports Summary
;;; ============================================================
;;;
;;; Thompson Construction:
;;;   rx->nfa, rx->dfa, rx->minimized-dfa
;;;
;;; Parsing:
;;;   parse-regex, regex-match?, regex-compile
;;;
;;; Low-level (for testing):
;;;   rx->fragment, fragment->fsm, thompson-reset-counter!
