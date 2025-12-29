;;; thimble/quill/runtime.ss — Quill runtime (minimal)
;;;
;;; Implements:
;;;   - quill-start: initialize a run
;;;   - quill-step: apply an intent and advance state/node deterministically
;;;   - quill-apply-effects: small effect language for early stories

;;; ============================================================
;;; Effects
;;; ============================================================

;;; Effect forms (initial set):
;;;   (set <var> <value>)
;;;   (inc <var> <delta>)
;;;   (flag <flag>)
;;;   (unflag <flag>)
;;;   (add-item <item>)
;;;   (remove-item <item>)
;;;   (goto <node-id>)
;;;   (end)

(define (quill-apply-effect story run eff)
  (let ([state (quill-run-state run)])
    (cond
      [(and (pair? eff) (eq? (car eff) 'set) (= (length eff) 3))
       (quill-run-with-state run (quill-state-set-var state (cadr eff) (caddr eff)))]

      [(and (pair? eff) (eq? (car eff) 'inc) (= (length eff) 3))
       (quill-run-with-state run (quill-state-inc-var state (cadr eff) (caddr eff)))]

      [(and (pair? eff) (eq? (car eff) 'flag) (= (length eff) 2))
       (quill-run-with-state run (quill-state-flag state (cadr eff)))]

      [(and (pair? eff) (eq? (car eff) 'unflag) (= (length eff) 2))
       (quill-run-with-state run (quill-state-unflag state (cadr eff)))]

      [(and (pair? eff) (eq? (car eff) 'add-item) (= (length eff) 2))
       (quill-run-with-state run (quill-state-add-item state (cadr eff)))]

      [(and (pair? eff) (eq? (car eff) 'remove-item) (= (length eff) 2))
       (quill-run-with-state run (quill-state-remove-item state (cadr eff)))]

      [(and (pair? eff) (eq? (car eff) 'goto) (= (length eff) 2))
       (if (quill-story-has-node? story (cadr eff))
           (quill-run-with-node run (cadr eff))
           (quill-run-with-message run (format "No such node: ~a" (cadr eff))))]

      [(or (eq? eff 'end) (and (pair? eff) (eq? (car eff) 'end)))
       (quill-run-with-done run #t)]

      [else
       (quill-run-with-message run (format "Unknown effect: ~a" eff))])))

(define (quill-apply-effects story run effects)
  (let loop ([r run] [effs effects])
    (if (null? effs)
        r
        (loop (quill-apply-effect story r (car effs)) (cdr effs)))))

;;; ============================================================
;;; Run lifecycle
;;; ============================================================

(define (quill-start story . state-opt)
  (let ([state (if (null? state-opt) (make-quill-state) (car state-opt))])
    (make-quill-run
      (quill-story-id story)
      (quill-story-start-node story)
      state
      '()
      #f
      #f)))

(define (quill-enter-node story run)
  (let ([node (quill-story-node story (quill-run-node-id run))])
    (if (not node)
        (quill-run-with-message run (format "Missing node: ~a" (quill-run-node-id run)))
        (quill-apply-effects story (quill-run-with-message run #f) (quill-node-on-enter node)))))

(define (quill-visible-choice-list story run)
  (let* ([node (quill-story-node story (quill-run-node-id run))]
         [state (quill-run-state run)])
    (if (not node)
        '()
        (quill-visible-choices node state))))

(define (quill-choose story run n)
  (let ([choices (quill-visible-choice-list story run)])
    (cond
      [(or (not (integer? n)) (< n 1) (> n (length choices)))
       (quill-run-with-message run "Invalid choice number.")]
      [else
       (let* ([choice (list-ref choices (- n 1))]
              [target (quill-choice-target choice)]
              [run1 (quill-run-with-message run #f)]
              [run2 (quill-apply-effects story run1 (quill-choice-effects choice))]
              [run3 (if (quill-story-has-node? story target)
                        (quill-run-with-node run2 target)
                        (quill-run-with-message run2 (format "No such node: ~a" target)))]
              [run4 (quill-enter-node story run3)])
         run4)])))

;;; quill-step : Story × Run × (String | Intent) -> (values Run String)
(define (quill-step story run input)
  (let* ([intent (if (string? input) (quill-parse input) input)]
         [run0 (if (quill-run-done? run)
                   run
                   (quill-run-with-message run #f))]
         [run1
          (cond
            [(quill-run-done? run0) run0]
            [(eq? (quill-intent-type intent) 'choose)
             (quill-choose story run0 (quill-intent-arg intent))]
            [(eq? (quill-intent-type intent) 'look)
             run0]
            [(eq? (quill-intent-type intent) 'help)
             (quill-run-with-message run0 "Enter a number to pick a choice. Commands: look, help, quit.")]
            [(eq? (quill-intent-type intent) 'quit)
             (quill-run-with-done run0 #t)]
            [else
             (quill-run-with-message run0 (format "I didn't understand: ~a" (quill-intent-arg intent)))] )]
         [out (quill-render story run1)]
         [run2 (quill-run-append-transcript run1 out)])
    (values run2 out)))

