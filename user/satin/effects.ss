;;; playpen/satin/effects.ss — Effect expression compilation
;;;
;;; Effects are state manipulation actions that compile to
;;; Quill runtime effect forms.

;;; ====
;;; Effect Forms (Satin DSL)
;;; ====

;;; Satin effect syntax:
;;;   (set-flag! <sym>)              ; set boolean flag
;;;   (clear-flag! <sym>)            ; clear flag
;;;   (toggle-flag! <sym>)           ; toggle flag
;;;   (give-item! <sym>)             ; add item to inventory
;;;   (take-item! <sym>)             ; remove item
;;;   (set-var! <sym> <val>)         ; set variable
;;;   (inc-var! <sym> [<n>])         ; increment (default 1)
;;;   (dec-var! <sym> [<n>])         ; decrement (default 1)
;;;   (print! <string>)              ; output message
;;;   (goto! <node>)                 ; immediate transition
;;;   (start-dialogue! <id>)         ; begin dialogue
;;;   (end-dialogue!)                ; end current dialogue
;;;   (activate-quest! <id>)         ; start quest
;;;   (complete-quest! <id>)         ; mark quest done
;;;   (fail-quest! <id>)             ; mark quest failed
;;;   (complete-step! <quest> <step>) ; complete a quest step
;;;   (advance-clock! [<n>])         ; advance time (default 1)
;;;   (mark-visited! <node>)         ; mark node as visited

;;; ====
;;; Effect Recognition
;;; ====

(define (satin-effect-form? expr)
  (and (pair? expr)
       (let ([tag (car (strip-span expr))])
            (memq tag '(set-flag! clear-flag! toggle-flag!
                        give-item! take-item!
                        set-var! inc-var! dec-var!
                        print! goto!
                        start-dialogue! end-dialogue!
                        activate-quest! complete-quest! fail-quest! complete-step!
                        advance-clock! mark-visited!
                        end)))))

;;; ====
;;; Effect Compilation
;;; ====

;;; satin-compile-effect : EffectExpr → QuillEffect
;;; Compile a Satin effect to a Quill runtime effect form.
(define (satin-compile-effect expr)
  (let ([raw (strip-span expr)])
       (if (not (pair? raw))
           'end
           (let ([tag (car raw)]
                 [args (cdr raw)])
                (case tag
                      ;; Flag operations
                      [(set-flag!)
                       (list 'set-flag (car args))]
                      
                      [(clear-flag!)
                       (list 'clear-flag (car args))]
                      
                      [(toggle-flag!)
                       (list 'toggle-flag (car args))]
                      
                      ;; Inventory operations
                      [(give-item!)
                       (list 'give-item (car args))]
                      
                      [(take-item!)
                       (list 'take-item (car args))]
                      
                      ;; Variable operations
                      [(set-var!)
                       (list 'set-var (car args) (cadr args))]
                      
                      [(inc-var!)
                       (let ([key (car args)]
                             [amt (if (pair? (cdr args)) (cadr args) 1)])
                            (list 'inc-var key amt))]
                      
                      [(dec-var!)
                       (let ([key (car args)]
                             [amt (if (pair? (cdr args)) (cadr args) 1)])
                            (list 'dec-var key amt))]
                      
                      ;; Output
                      [(print!)
                       (list 'print (car args))]
                      
                      ;; Navigation
                      [(goto!)
                       (list 'goto (car args))]
                      
                      ;; Dialogue
                      [(start-dialogue!)
                       (list 'start-dialogue (car args))]
                      
                      [(end-dialogue!)
                       (list 'end-dialogue)]
                      
                      ;; Quest operations
                      [(activate-quest!)
                       (list 'activate-quest (car args))]
                      
                      [(complete-quest!)
                       (list 'complete-quest (car args))]
                      
                      [(fail-quest!)
                       (list 'fail-quest (car args))]
                      
                      [(complete-step!)
                       (list 'complete-step (car args) (cadr args))]
                      
                      ;; Time
                      [(advance-clock!)
                       (let ([amt (if (pair? args) (car args) 1)])
                            (list 'advance-clock amt))]
                      
                      ;; Visited tracking
                      [(mark-visited!)
                       (list 'set-flag (satin-visited-flag (car args)))]
                      
                      ;; End marker
                      [(end)
                       'end]
                      
                      ;; Unknown - pass through
                      [else raw])))))

;;; satin-compile-effects : (List EffectExpr) → (List QuillEffect)
;;; Compile a list of effects.
(define (satin-compile-effects exprs)
  (map satin-compile-effect exprs))

;;; ====
;;; Effect Validation
;;; ====

;;; satin-effect-valid? : EffectExpr → Bool
;;; Check if an effect form is syntactically valid.
(define (satin-effect-valid? expr)
  (let ([raw (strip-span expr)])
       (cond
        [(eq? raw 'end) #t]
        [(not (pair? raw)) #f]
        [else
         (let ([tag (car raw)]
               [args (cdr raw)])
              (case tag
                    ;; Single symbol argument
                    [(set-flag! clear-flag! toggle-flag! give-item! take-item!
                                start-dialogue! activate-quest! complete-quest! fail-quest!
                                goto! mark-visited!)
                     (and (pair? args)
                          (symbol? (car args)))]
                    
                    ;; Two symbol arguments (quest step completion)
                    [(complete-step!)
                     (and (pair? args)
                          (symbol? (car args))
                          (pair? (cdr args))
                          (symbol? (cadr args)))]
                    
                    ;; Two arguments: symbol + value
                    [(set-var!)
                     (and (pair? args)
                          (symbol? (car args))
                          (pair? (cdr args)))]
                    
                    ;; Symbol + optional number
                    [(inc-var! dec-var!)
                     (and (pair? args)
                          (symbol? (car args))
                          (or (null? (cdr args))
                              (number? (cadr args))))]
                    
                    ;; Optional number
                    [(advance-clock!)
                     (or (null? args)
                         (number? (car args)))]
                    
                    ;; String argument
                    [(print!)
                     (and (pair? args)
                          (string? (car args)))]
                    
                    ;; No arguments
                    [(end-dialogue! end)
                     #t]
                    
                    [else #f]))])))

;;; satin-effect-describe : EffectExpr → String
;;; Generate human-readable description of effect.
(define (satin-effect-describe expr)
  (let ([raw (strip-span expr)])
       (cond
        [(eq? raw 'end) "end story"]
        [(not (pair? raw)) "unknown effect"]
        [else
         (let ([tag (car raw)]
               [args (cdr raw)])
              (case tag
                    [(set-flag!) (format "set flag ~a" (car args))]
                    [(clear-flag!) (format "clear flag ~a" (car args))]
                    [(toggle-flag!) (format "toggle flag ~a" (car args))]
                    [(give-item!) (format "give item ~a" (car args))]
                    [(take-item!) (format "take item ~a" (car args))]
                    [(set-var!) (format "set ~a to ~a" (car args) (cadr args))]
                    [(inc-var!) (format "increment ~a" (car args))]
                    [(dec-var!) (format "decrement ~a" (car args))]
                    [(print!) (format "print ~s" (car args))]
                    [(goto!) (format "go to node ~a" (car args))]
                    [(start-dialogue!) (format "start dialogue ~a" (car args))]
                    [(end-dialogue!) "end dialogue"]
                    [(activate-quest!) (format "activate quest ~a" (car args))]
                    [(complete-quest!) (format "complete quest ~a" (car args))]
                    [(fail-quest!) (format "fail quest ~a" (car args))]
                    [(complete-step!) (format "complete step ~a in quest ~a" (cadr args) (car args))]
                    [(advance-clock!) "advance clock"]
                    [(mark-visited!) (format "mark ~a visited" (car args))]
                    [(end) "end story"]
                    [else (format "~a" raw)]))])))
