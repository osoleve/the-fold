;;; playpen/quill/types.ss — Quill core types
;;;
;;; This file defines the core data structures for Quill:
;;;   - Story: a directed graph of nodes/scenes
;;;   - Node: narrative content + choices + on-enter effects
;;;   - Choice: labeled transition with optional guard + effects
;;;   - Run: current node + state + transcript + message buffer
;;;
;;; Design constraints:
;;;   - Deterministic by default (no ambient randomness/time)
;;;   - Data-first structures (easy to validate/compile from DSL later)

(define *quill-version* "0.1.0")

;;; ============================================================
;;; Story / Node / Choice
;;; ============================================================

(define-record-type quill-story%
  (fields id title start-node nodes meta))

(define make-quill-story make-quill-story%)
(define quill-story-id quill-story%-id)
(define quill-story-title quill-story%-title)
(define quill-story-start-node quill-story%-start-node)
(define quill-story-nodes quill-story%-nodes)
(define quill-story-meta quill-story%-meta)

;;; nodes is an alist: ((node-id . quill-node) ...)
(define (quill-story-node story node-id)
  (let ([pair (assq node-id (quill-story-nodes story))])
       (and pair (cdr pair))))

(define (quill-story-has-node? story node-id)
  (and (quill-story-node story node-id) #t))

(define-record-type quill-node%
  (fields id title body choices on-enter meta))

(define make-quill-node make-quill-node%)
(define quill-node-id quill-node%-id)
(define quill-node-title quill-node%-title)
(define quill-node-body quill-node%-body)
(define quill-node-choices quill-node%-choices)
(define quill-node-on-enter quill-node%-on-enter)
(define quill-node-meta quill-node%-meta)

(define-record-type quill-choice%
  (fields label target guard effects meta))

(define make-quill-choice make-quill-choice%)
(define quill-choice-label quill-choice%-label)
(define quill-choice-target quill-choice%-target)
(define quill-choice-guard quill-choice%-guard)
(define quill-choice-effects quill-choice%-effects)
(define quill-choice-meta quill-choice%-meta)

;;; ============================================================
;;; Run State
;;; ============================================================

(define-record-type quill-run%
  (fields story-id node-id state transcript done? message))

(define make-quill-run make-quill-run%)
(define quill-run-story-id quill-run%-story-id)
(define quill-run-node-id quill-run%-node-id)
(define quill-run-state quill-run%-state)
(define quill-run-transcript quill-run%-transcript)
(define quill-run-done? quill-run%-done?)
(define quill-run-message quill-run%-message)

;;; Constructors / helpers
(define (quill-run-with-node run node-id)
  (make-quill-run (quill-run-story-id run)
                  node-id
                  (quill-run-state run)
                  (quill-run-transcript run)
                  (quill-run-done? run)
                  (quill-run-message run)))

(define (quill-run-with-state run state)
  (make-quill-run (quill-run-story-id run)
                  (quill-run-node-id run)
                  state
                  (quill-run-transcript run)
                  (quill-run-done? run)
                  (quill-run-message run)))

(define (quill-run-with-done run done?)
  (make-quill-run (quill-run-story-id run)
                  (quill-run-node-id run)
                  (quill-run-state run)
                  (quill-run-transcript run)
                  done?
                  (quill-run-message run)))

(define (quill-run-with-message run msg)
  (make-quill-run (quill-run-story-id run)
                  (quill-run-node-id run)
                  (quill-run-state run)
                  (quill-run-transcript run)
                  (quill-run-done? run)
                  msg))

(define (quill-run-append-transcript run entry)
  (make-quill-run (quill-run-story-id run)
                  (quill-run-node-id run)
                  (quill-run-state run)
                  (cons entry (quill-run-transcript run))
                  (quill-run-done? run)
                  (quill-run-message run)))

;;; Transcript entry helpers
;;; Entries are intended to be stable, structured data for assessment/debugging.
;;; A typical entry is:
;;;   ((kind . turn) (node . <symbol>) (input . <string>) (intent . <sexp>) (output . <string>))
(define (quill-transcript-entry kind fields)
  (cons (cons 'kind kind) fields))

;;; ============================================================
;;; Intent (input model)
;;; ============================================================

;;; Intent values are small tagged lists:
;;;   (choose n)
;;;   (look) | (examine String)
;;;   (move Symbol)
;;;   (inventory)
;;;   (take String) | (drop String)
;;;   (use String String|#f)
;;;   (say String)
;;;   (help) | (quit)
;;;   (unknown raw)

(define (quill-intent-choose n) (list 'choose n))
(define (quill-intent-look) (list 'look))
(define (quill-intent-examine obj) (list 'examine obj))
(define (quill-intent-move dir) (list 'move dir))
(define (quill-intent-inventory) (list 'inventory))
(define (quill-intent-take obj) (list 'take obj))
(define (quill-intent-drop obj) (list 'drop obj))
(define (quill-intent-use obj target) (list 'use obj target))
(define (quill-intent-say text) (list 'say text))
(define (quill-intent-help) (list 'help))
(define (quill-intent-quit) (list 'quit))
(define (quill-intent-unknown raw) (list 'unknown raw))

(define (quill-intent-type intent) (car intent))
(define (quill-intent-args intent) (cdr intent))
(define (quill-intent-arg intent) (and (pair? (cdr intent)) (cadr intent)))
