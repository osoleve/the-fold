;;; shell/meta.ss --- Inline Metadata Tag Parser
;;;
;;; Tags are inline markers that travel with text:
;;;   @tag           - Simple boolean tag (e.g., @draft, @reviewed)
;;;   @key:value     - Key-value pair (e.g., @priority:high, @status:complete)
;;;   @see:path      - Reference to another file/resource
;;;   @ref:id        - Reference to an ID
;;;
;;; This is Shell code: parsing text from Outside, handling edge cases.
;;;
;;; Design principles:
;;;   - Tags start with @ and are at word boundaries
;;;   - Email addresses (user@domain.com) are NOT tags
;;;   - Tags are alphanumeric with hyphens/underscores allowed
;;;   - Values in key:value can include paths, IDs, etc.
;;;
;;; Dependencies: core/base/prelude.ss

(load "core/base/prelude.ss")

;;; ============================================================
;;; Character Classification for Tags
;;; ============================================================

;;; tag-start-char? : Char -> Boolean
;;; Characters that can start a tag name (after @)
(define (tag-start-char? c)
  (or (char-alphabetic? c)
      (char=? c #\_)))

;;; tag-char? : Char -> Boolean
;;; Characters allowed in tag names
(define (tag-char? c)
  (or (char-alphabetic? c)
      (char-numeric? c)
      (char=? c #\-)
      (char=? c #\_)))

;;; value-char? : Char -> Boolean
;;; Characters allowed in tag values (more permissive)
;;; Allows paths, IDs, etc. but stops at whitespace
(define (value-char? c)
  (and (not (char-whitespace? c))
       (not (char=? c #\@))))  ; Don't eat into next tag

;;; word-boundary-before? : List Char -> Boolean
;;; Is there a word boundary before current position?
;;; Empty list or whitespace before means boundary.
(define (word-boundary-before? preceding-chars)
  (or (null? preceding-chars)
      (char-whitespace? (car preceding-chars))
      ;; Also treat certain punctuation as boundaries
      (memq (car preceding-chars) '(#\( #\[ #\{ #\" #\' #\< #\newline))))

;;; ============================================================
;;; Email Address Detection
;;; ============================================================

;;; Heuristic: if we see @something and it looks like domain.tld, skip it
;;; We look ahead for domain-like patterns

;;; looks-like-email-domain? : List Char -> Boolean
;;; Check if the characters after @ look like an email domain
(define (looks-like-email-domain? chars)
  (let loop ([cs chars] [seen-alpha #f] [seen-dot #f])
       (cond
        [(null? cs) (and seen-alpha seen-dot)]
        [(char=? (car cs) #\.)
         (if seen-alpha
             (loop (cdr cs) #f #t)  ; Reset alpha, mark dot
             #f)]  ; Dot without preceding alpha = not domain
        [(or (char-alphabetic? (car cs))
             (char-numeric? (car cs))
             (char=? (car cs) #\-))
         (loop (cdr cs) #t seen-dot)]
        [(char-whitespace? (car cs))
         (and seen-alpha seen-dot)]  ; End of potential domain
        [else
         (and seen-alpha seen-dot)])))  ; Hit other char, check if valid so far

;;; preceded-by-email-local? : List Char -> Boolean
;;; Check if chars before @ look like email local part
(define (preceded-by-email-local? preceding)
  (and (not (null? preceding))
       (not (char-whitespace? (car preceding)))
       ;; Email local parts contain alphanumeric, dots, etc.
       (let ([c (car preceding)])
            (or (char-alphabetic? c)
                (char-numeric? c)
                (memq c '(#\. #\- #\_ #\+))))))

;;; ============================================================
;;; Tag Parsing
;;; ============================================================

;;; parse-tag-name : List Char -> (name-string . remaining-chars) | #f
;;; Parse a tag name starting from the character after @
(define (parse-tag-name chars)
  (if (or (null? chars) (not (tag-start-char? (car chars))))
      #f
      (let loop ([cs chars] [acc '()])
           (cond
            [(null? cs)
             (cons (list->string (reverse acc)) '())]
            [(tag-char? (car cs))
             (loop (cdr cs) (cons (car cs) acc))]
            [else
             (cons (list->string (reverse acc)) cs)]))))

;;; parse-tag-value : List Char -> (value-string . remaining-chars)
;;; Parse a tag value (after the colon)
(define (parse-tag-value chars)
  (let loop ([cs chars] [acc '()])
       (cond
        [(null? cs)
         (cons (list->string (reverse acc)) '())]
        [(value-char? (car cs))
         (loop (cdr cs) (cons (car cs) acc))]
        [else
         (cons (list->string (reverse acc)) cs)])))

;;; parse-one-tag : List Char -> (tag . remaining) | #f
;;; Attempt to parse a tag starting with @
;;; Returns ((key . value) . remaining-chars) or ((key . #t) . remaining-chars)
;;; or #f if not a valid tag
(define (parse-one-tag chars preceding)
  (cond
   ;; Must start with @
   [(or (null? chars) (not (char=? (car chars) #\@)))
    #f]
   ;; Check word boundary
   [(not (word-boundary-before? preceding))
    #f]
   ;; Check for email pattern
   [(and (preceded-by-email-local? preceding)
         (looks-like-email-domain? (cdr chars)))
    #f]
   [else
    (let ([name-result (parse-tag-name (cdr chars))])
         (if (not name-result)
             #f
             (let ([name (car name-result)]
                   [rest (cdr name-result)])
                  ;; Empty name is invalid
                  (if (string=? name "")
                      #f
                      ;; Check for colon (key:value)
                      (if (and (not (null? rest))
                               (char=? (car rest) #\:))
                          ;; Parse value
                          (let ([value-result (parse-tag-value (cdr rest))])
                               (if (string=? (car value-result) "")
                                   ;; Empty value - treat as simple tag
                                   (cons (cons name #t) rest)
                                   (cons (cons name (car value-result))
                                         (cdr value-result))))
                          ;; Simple tag
                          (cons (cons name #t) rest))))))]))

;;; ============================================================
;;; Main Extraction
;;; ============================================================

;;; extract-tags : String -> Alist
;;; Extract all tags from text, returning an association list.
;;; Multiple tags with same key are preserved (later values come first in result).
(define (extract-tags text)
  (let ([chars (string->list text)])
       (let loop ([cs chars] [preceding '()] [tags '()])
            (cond
             [(null? cs)
              (reverse tags)]
             [(char=? (car cs) #\@)
              (let ([result (parse-one-tag cs preceding)])
                   (if result
                       (loop (cdr result) (list (car cs)) (cons (car result) tags))
                       (loop (cdr cs) (list (car cs)) tags)))]
             [else
              (loop (cdr cs) (list (car cs)) tags)]))))

;;; ============================================================
;;; Query Functions
;;; ============================================================

;;; has-tag? : String x String -> Boolean
;;; Check if text contains a specific tag (by name).
(define (has-tag? text tag-name)
  (let ([tags (extract-tags text)])
       (if (assoc tag-name tags) #t #f)))

;;; get-tag-value : String x String -> String | #t | #f
;;; Get the value for a tag. Returns:
;;;   - The value string for key:value tags
;;;   - #t for simple tags (no value)
;;;   - #f if tag not found
(define (get-tag-value text key)
  (let ([tags (extract-tags text)])
       (let ([entry (assoc key tags)])
            (if entry
                (cdr entry)
                #f))))

;;; get-all-tag-values : String x String -> List
;;; Get all values for a tag (for tags that appear multiple times).
(define (get-all-tag-values text key)
  (let ([tags (extract-tags text)])
       (fold-right
        (lambda (entry acc)
                (if (string=? (car entry) key)
                    (cons (cdr entry) acc)
                    acc))
        '()
        tags)))

;;; ============================================================
;;; Text Manipulation
;;; ============================================================

;;; strip-tags : String -> String
;;; Remove all tags from text, leaving clean prose.
(define (strip-tags text)
  (let ([chars (string->list text)])
       (let loop ([cs chars] [preceding '()] [acc '()])
            (cond
             [(null? cs)
              (list->string (reverse acc))]
             [(char=? (car cs) #\@)
              (let ([result (parse-one-tag cs preceding)])
                   (if result
                       ;; Skip the tag, continue after it
                       ;; Keep preceding as the char before the @ for next potential tag
                       (loop (cdr result) preceding acc)
                       ;; Not a tag, keep the @
                       (loop (cdr cs) (list (car cs)) (cons (car cs) acc))))]
             [else
              (loop (cdr cs) (list (car cs)) (cons (car cs) acc))]))))

;;; highlight-tags : String -> String
;;; Return text with tags highlighted using brackets.
;;; Example: "@draft" -> "[TAG:draft]"
;;;          "@status:complete" -> "[TAG:status=complete]"
(define (highlight-tags text)
  (let ([chars (string->list text)])
       (let loop ([cs chars] [preceding '()] [acc '()])
            (cond
             [(null? cs)
              (list->string (reverse acc))]
             [(char=? (car cs) #\@)
              (let ([result (parse-one-tag cs preceding)])
                   (if result
                       ;; Replace tag with highlighted version
                       (let* ([tag (car result)]
                              [key (car tag)]
                              [val (cdr tag)]
                              [highlighted
                               (if (eq? val #t)
                                   (string-append "[TAG:" key "]")
                                   (string-append "[TAG:" key "=" val "]"))]
                              [hl-chars (string->list highlighted)]
                              ;; Last char of highlighted becomes new preceding
                              [new-preceding (if (null? hl-chars)
                                                 preceding
                                                 (list (car (reverse hl-chars))))])
                             (loop (cdr result)
                                   new-preceding
                                   (append (reverse hl-chars) acc)))
                       ;; Not a tag, keep the @
                       (loop (cdr cs) (list (car cs)) (cons (car cs) acc))))]
             [else
              (loop (cdr cs) (list (car cs)) (cons (car cs) acc))]))))

;;; ============================================================
;;; Tag Categories (Convenience)
;;; ============================================================

;;; Common tag categories for The Fold

;;; status-tags : List String
;;; Standard status indicators
(define status-tags '("draft" "reviewed" "complete" "deprecated" "wip"))

;;; priority-tags : List String
;;; Standard priority values
(define priority-tags '("low" "medium" "high" "critical"))

;;; get-status : String -> String | #f
;;; Get the status tag value, checking both @status:X and @X for status words
(define (get-status text)
  (let ([explicit (get-tag-value text "status")])
       (if explicit
           explicit
           ;; Check for bare status tags
           (let loop ([statuses status-tags])
                (if (null? statuses)
                    #f
                    (if (has-tag? text (car statuses))
                        (car statuses)
                        (loop (cdr statuses))))))))

;;; get-priority : String -> String | #f
;;; Get the priority tag value
(define (get-priority text)
  (get-tag-value text "priority"))

;;; get-references : String -> List String
;;; Get all @see: and @ref: values
(define (get-references text)
  (append (get-all-tag-values text "see")
          (get-all-tag-values text "ref")))

;;; ============================================================
;;; Predicate Helpers
;;; ============================================================

;;; is-draft? : String -> Boolean
(define (is-draft? text)
  (or (has-tag? text "draft")
      (equal? (get-tag-value text "status") "draft")))

;;; is-complete? : String -> Boolean
(define (is-complete? text)
  (or (has-tag? text "complete")
      (equal? (get-tag-value text "status") "complete")))

;;; is-todo? : String -> Boolean
(define (is-todo? text)
  (has-tag? text "todo"))

;;; is-high-priority? : String -> Boolean
(define (is-high-priority? text)
  (let ([p (get-priority text)])
       (or (equal? p "high")
           (equal? p "critical"))))
