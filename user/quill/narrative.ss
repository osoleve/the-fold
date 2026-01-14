;;; playpen/quill/narrative.ss — Quill narrative engine helpers
;;;
;;; Provides data structures + helpers for rules, dialogue trees, quests,
;;; and timelines. These are pure utilities that can be layered into
;;; runtime/authoring tools.

;;; ====
;;; Records
;;; ====

;;; Rules
;;;   guard: (state -> bool) | #t | #f | #f
;;;   effects: list of Quill effects
;;;   once?: only fire once
(define-record-type quill-rule%
  (fields id guard effects once? meta))

(define make-quill-rule make-quill-rule%)
(define quill-rule-id quill-rule%-id)
(define quill-rule-guard quill-rule%-guard)
(define quill-rule-effects quill-rule%-effects)
(define quill-rule-once? quill-rule%-once?)
(define quill-rule-meta quill-rule%-meta)

;;; Dialogue
(define-record-type quill-dialogue%
  (fields id nodes meta))

(define make-quill-dialogue make-quill-dialogue%)
(define quill-dialogue-id quill-dialogue%-id)
(define quill-dialogue-nodes quill-dialogue%-nodes)
(define quill-dialogue-meta quill-dialogue%-meta)

(define-record-type quill-dialogue-node%
  (fields id speaker text choices on-enter meta))

(define make-quill-dialogue-node make-quill-dialogue-node%)
(define quill-dialogue-node-id quill-dialogue-node%-id)
(define quill-dialogue-node-speaker quill-dialogue-node%-speaker)
(define quill-dialogue-node-text quill-dialogue-node%-text)
(define quill-dialogue-node-choices quill-dialogue-node%-choices)
(define quill-dialogue-node-on-enter quill-dialogue-node%-on-enter)
(define quill-dialogue-node-meta quill-dialogue-node%-meta)

(define-record-type quill-dialogue-choice%
  (fields label target guard effects meta))

(define make-quill-dialogue-choice make-quill-dialogue-choice%)
(define quill-dialogue-choice-label quill-dialogue-choice%-label)
(define quill-dialogue-choice-target quill-dialogue-choice%-target)
(define quill-dialogue-choice-guard quill-dialogue-choice%-guard)
(define quill-dialogue-choice-effects quill-dialogue-choice%-effects)
(define quill-dialogue-choice-meta quill-dialogue-choice%-meta)

;;; Quests
(define-record-type quill-quest%
  (fields id title steps on-complete meta))

(define make-quill-quest make-quill-quest%)
(define quill-quest-id quill-quest%-id)
(define quill-quest-title quill-quest%-title)
(define quill-quest-steps quill-quest%-steps)
(define quill-quest-on-complete quill-quest%-on-complete)
(define quill-quest-meta quill-quest%-meta)

(define-record-type quill-quest-step%
  (fields id description guard meta))

(define make-quill-quest-step make-quill-quest-step%)
(define quill-quest-step-id quill-quest-step%-id)
(define quill-quest-step-description quill-quest-step%-description)
(define quill-quest-step-guard quill-quest-step%-guard)
(define quill-quest-step-meta quill-quest-step%-meta)

;;; Timeline events
(define-record-type quill-timeline-event%
  (fields id time effects once? meta))

(define make-quill-timeline-event make-quill-timeline-event%)
(define quill-timeline-event-id quill-timeline-event%-id)
(define quill-timeline-event-time quill-timeline-event%-time)
(define quill-timeline-event-effects quill-timeline-event%-effects)
(define quill-timeline-event-once? quill-timeline-event%-once?)
(define quill-timeline-event-meta quill-timeline-event%-meta)

;;; ====
;;; Small utilities
;;; ====

(define (quill-narrative-alist-ref alist key default)
  (let ([p (assq key alist)])
       (if p (cdr p) default)))

(define (quill-guard-true? guard state)
  (cond
   [(not guard) #t]
   [(eq? guard #t) #t]
   [(eq? guard #f) #f]
   [(procedure? guard) (and (guard state) #t)]
   [else #t]))

(define (quill-story-meta-get story key default)
  (let ([p (assq key (quill-story-meta story))])
       (if p (cdr p) default)))

(define (quill-story-rules story)
  (quill-story-meta-get story 'rules '()))

(define (quill-story-dialogues story)
  (quill-story-meta-get story 'dialogues '()))

(define (quill-story-quests story)
  (quill-story-meta-get story 'quests '()))

(define (quill-story-timeline story)
  (quill-story-meta-get story 'timeline '()))

;;; ====
;;; Rules
;;; ====

(define (quill-run-fired-rules run)
  (quill-state-meta-get (quill-run-state run) 'rules-fired '()))

(define (quill-run-rule-fired? run rule-id)
  (and (memq rule-id (quill-run-fired-rules run)) #t))

(define (quill-run-mark-rule-fired run rule-id)
  (let* ([fired (quill-run-fired-rules run)]
         [state1 (quill-state-meta-set (quill-run-state run)
                                       'rules-fired
                                       (cons rule-id fired))])
        (quill-run-with-state run state1)))

(define (quill-apply-rule story run rule apply-effects)
  (let* ([state (quill-run-state run)]
         [id (quill-rule-id rule)]
         [guard (quill-rule-guard rule)]
         [once? (quill-rule-once? rule)]
         [should-fire? (quill-guard-true? guard state)]
         [already? (and once? (quill-run-rule-fired? run id))])
        (if (and should-fire? (not already?))
            (let* ([run1 (apply-effects story run (or (quill-rule-effects rule) '()))]
                   [run2 (if once? (quill-run-mark-rule-fired run1 id) run1)])
                  run2)
            run)))

(define (quill-apply-rules story run rules apply-effects)
  (let loop ([r run] [rs rules])
       (if (null? rs)
           r
           (loop (quill-apply-rule story r (car rs) apply-effects) (cdr rs)))))

;;; ====
;;; Timeline
;;; ====

(define (quill-run-clock run)
  (quill-state-meta-get (quill-run-state run) 'clock 0))

(define (quill-run-clock-set run value)
  (let ([state1 (quill-state-meta-set (quill-run-state run) 'clock value)])
       (quill-run-with-state run state1)))

(define (quill-run-advance-clock run delta)
  (quill-run-clock-set run (+ (quill-run-clock run) delta)))

(define (quill-run-fired-events run)
  (quill-state-meta-get (quill-run-state run) 'timeline-fired '()))

(define (quill-run-event-fired? run event-id)
  (and (memq event-id (quill-run-fired-events run)) #t))

(define (quill-run-mark-event-fired run event-id)
  (let* ([fired (quill-run-fired-events run)]
         [state1 (quill-state-meta-set (quill-run-state run)
                                       'timeline-fired
                                       (cons event-id fired))])
        (quill-run-with-state run state1)))

(define (quill-apply-timeline story run events apply-effects)
  (let ([clock (quill-run-clock run)])
       (let loop ([r run] [evs events])
            (if (null? evs)
                r
                (let* ([ev (car evs)]
                       [time (quill-timeline-event-time ev)]
                       [id (quill-timeline-event-id ev)]
                       [once? (quill-timeline-event-once? ev)]
                       [should-fire? (and (number? time) (<= time clock))]
                       [already? (and once? (quill-run-event-fired? r id))]
                       [r2 (if (and should-fire? (not already?))
                               (let* ([r1 (apply-effects story r (or (quill-timeline-event-effects ev) '()))]
                                      [r3 (if once? (quill-run-mark-event-fired r1 id) r1)])
                                     r3)
                               r)])
                      (loop r2 (cdr evs)))))))

;;; ====
;;; Quests
;;; ====

(define (quill-run-quest-statuses run)
  (quill-state-meta-get (quill-run-state run) 'quests '()))

(define (quill-quest-status run quest-id)
  (let* ([qs (quill-run-quest-statuses run)]
         [p (assq quest-id qs)])
        (if p (cdr p) 'inactive)))

(define (quill-quest-set-status run quest-id status)
  (let* ([qs (quill-run-quest-statuses run)]
         [p (assq quest-id qs)]
         [qs2 (if p
                  (cons (cons quest-id status) (remq p qs))
                  (cons (cons quest-id status) qs))]
         [state1 (quill-state-meta-set (quill-run-state run) 'quests qs2)])
        (quill-run-with-state run state1)))

(define (quill-quest-activate run quest-id)
  (quill-quest-set-status run quest-id 'active))

(define (quill-quest-complete run quest-id)
  (quill-quest-set-status run quest-id 'complete))

(define (quill-quest-fail run quest-id)
  (quill-quest-set-status run quest-id 'failed))

(define (quill-quest-complete? quest state)
  (let ([steps (quill-quest-steps quest)])
       (andmap (lambda (step)
                       (quill-guard-true? (quill-quest-step-guard step) state))
               steps)))

(define (quill-update-quests story run quests apply-effects)
  (let loop ([r run] [qs quests])
       (if (null? qs)
           r
           (let* ([quest (car qs)]
                  [id (quill-quest-id quest)]
                  [status (quill-quest-status r id)]
                  [state (quill-run-state r)]
                  [completed? (and (eq? status 'active)
                                   (quill-quest-complete? quest state))]
                  [r1 (if completed?
                          (let* ([r0 (quill-quest-complete r id)]
                                 [r2 (apply-effects story r0 (or (quill-quest-on-complete quest) '()))])
                                r2)
                          r)])
                 (loop r1 (cdr qs))))))

;;; ====
;;; Dialogue
;;; ====

(define (quill-story-dialogue story dialogue-id)
  (let loop ([ds (quill-story-dialogues story)])
       (cond
        [(null? ds) #f]
        [(eq? (quill-dialogue-id (car ds)) dialogue-id) (car ds)]
        [else (loop (cdr ds))])))

(define (quill-dialogue-node dialogue node-id)
  (let ([p (assq node-id (quill-dialogue-nodes dialogue))])
       (and p (cdr p))))

(define (quill-dialogue-start-node dialogue)
  (quill-narrative-alist-ref (quill-dialogue-meta dialogue) 'start 'start))

(define (quill-run-dialogue run)
  (quill-state-meta-get (quill-run-state run) 'dialogue #f))

(define (quill-dialogue-active? run)
  (and (pair? (quill-run-dialogue run)) #t))

(define (quill-run-with-dialogue run dialogue-id node-id)
  (let ([state1 (quill-state-meta-set (quill-run-state run)
                                      'dialogue
                                      (cons dialogue-id node-id))])
       (quill-run-with-state run state1)))

(define (quill-run-clear-dialogue run)
  (let ([state1 (quill-state-meta-remove (quill-run-state run) 'dialogue)])
       (quill-run-with-state run state1)))

(define (quill-dialogue-current-node story run)
  (let* ([pair (quill-run-dialogue run)]
         [dialogue-id (and (pair? pair) (car pair))]
         [node-id (and (pair? pair) (cdr pair))]
         [dialogue (and dialogue-id (quill-story-dialogue story dialogue-id))])
        (and dialogue node-id (quill-dialogue-node dialogue node-id))))

(define (quill-dialogue-choice-visible? choice state)
  (let ([g (quill-dialogue-choice-guard choice)])
       (quill-guard-true? g state)))

(define (quill-dialogue-visible-choices node state)
  (filter (lambda (c) (quill-dialogue-choice-visible? c state))
          (quill-dialogue-node-choices node)))

(define (quill-dialogue-enter story run dialogue node apply-effects)
  (let* ([run1 (quill-run-with-message run #f)]
         [run2 (quill-run-with-dialogue run1
                                        (quill-dialogue-id dialogue)
                                        (quill-dialogue-node-id node))]
         [run3 (apply-effects story run2 (or (quill-dialogue-node-on-enter node) '()))])
        run3))

(define (quill-dialogue-start story run dialogue-id apply-effects)
  (let* ([dialogue (quill-story-dialogue story dialogue-id)]
         [node-id (and dialogue (quill-dialogue-start-node dialogue))]
         [node (and dialogue node-id (quill-dialogue-node dialogue node-id))])
        (if (and dialogue node)
            (quill-dialogue-enter story run dialogue node apply-effects)
            (quill-run-with-message run (format "No such dialogue: ~a" dialogue-id)))))

(define (quill-dialogue-end run)
  (quill-run-clear-dialogue run))

(define (quill-dialogue-choose story run n apply-effects)
  (let* ([node (quill-dialogue-current-node story run)]
         [state (quill-run-state run)]
         [choices (if node (quill-dialogue-visible-choices node state) '())])
        (cond
         [(not node)
          (quill-run-with-message run "No active dialogue.")]
         [(or (not (integer? n)) (< n 1) (> n (length choices)))
          (quill-run-with-message run "Invalid dialogue choice.")]
         [else
          (let* ([choice (list-ref choices (- n 1))]
                 [target (quill-dialogue-choice-target choice)]
                 [run1 (quill-run-with-message run #f)]
                 [run2 (apply-effects story run1 (or (quill-dialogue-choice-effects choice) '()))]
                 [dialogue-id (car (quill-run-dialogue run))]
                 [dialogue (quill-story-dialogue story dialogue-id)])
                (cond
                 [(or (not target) (eq? target 'end))
                  (quill-dialogue-end run2)]
                 [(and dialogue (quill-dialogue-node dialogue target))
                  (quill-dialogue-enter story run2 dialogue (quill-dialogue-node dialogue target) apply-effects)]
                 [else
                  (quill-run-with-message run2 (format "No such dialogue node: ~a" target))]))])))

(define (quill-dialogue-handle story run intent apply-effects)
  (let ([kind (quill-intent-type intent)])
       (cond
        [(eq? kind 'choose)
         (quill-dialogue-choose story run (quill-intent-arg intent) apply-effects)]
        [(eq? kind 'help)
         (quill-run-with-message run "Dialogue: choose a number, or type 'quit'.")]
        [(eq? kind 'quit)
         (quill-run-with-done run #t)]
        [else
         (quill-run-with-message run "Dialogue expects a choice number.")]))
  )

;;; ====
;;; Narrative advance
;;; ====

(define (quill-narrative-advance story run apply-effects . options-opt)
  (let* ([options (if (null? options-opt) '() (car options-opt))]
         [tick? (quill-narrative-alist-ref options 'tick? #t)]
         [delta (quill-narrative-alist-ref options 'tick-delta 1)]
         [run1 (if tick? (quill-run-advance-clock run delta) run)]
         [run2 (quill-apply-timeline story run1 (quill-story-timeline story) apply-effects)]
         [run3 (quill-apply-rules story run2 (quill-story-rules story) apply-effects)]
         [run4 (quill-update-quests story run3 (quill-story-quests story) apply-effects)])
        run4))
