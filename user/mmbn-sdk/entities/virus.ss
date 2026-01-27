;;; user/mmbn-sdk/entities/virus.ss — Virus Enemies
;;; @module mmbn/virus
;;; @requires entity, sprite-designer
;;; Load via: (load "user/mmbn-sdk/mmbn.ss")

;; Dependencies loaded by mmbn.ss

(doc 'module 'mmbn/virus)
(doc 'description "Virus enemy entities. Viruses are hostile programs that
inhabit the Net and attack Navis. Each virus type has unique behavior and attacks.")
(doc 'layer 'user)
(doc 'purity 'partial)

;;; ============================================================
;;; Mettaur Virus (Classic hardhat virus)
;;; ============================================================

(doc 'section 'mettaur)

(define *mettaur-palette*
  `((#\. . ,color-default)
    (#\Y . ,(rgb 255 255 0))         ; Yellow body
    (#\y . ,(rgb 200 180 0))         ; Dark yellow
    (#\W . ,(rgb 255 255 255))       ; White
    (#\# . ,(rgb 0 0 0))             ; Black
    (#\G . ,(rgb 100 100 100))       ; Gray (pickaxe)
    (#\g . ,(rgb 60 60 60))))        ; Dark gray

(define mettaur-sprite-idle
  (define-sprite-from-strings 8 8
    '("..YYYY.."
      ".YYYYYY."
      "YY#YY#YY"
      "YYYYYYYY"
      ".yYYYYy."
      "..yyyy.."
      ".GGggGG."
      "GG....GG")
    *mettaur-palette*))

(define mettaur-sprite-guard
  (define-sprite-from-strings 8 8
    '("..YYYY.."
      ".YYYYYY."
      "YYYYYYYY"  ; Eyes hidden under helmet
      "YYYYYYYY"
      ".yYYYYy."
      "..yyyy.."
      ".GGggGG."
      "GG....GG")
    *mettaur-palette*))

(define mettaur-sprite-attack
  (define-sprite-from-strings 12 8
    '("..YYYY......"
      ".YYYYYY....."
      "YY#YY#YY...."
      "YYYYYYYY.GGG"
      ".yYYYYy.GGGG"
      "..yyyy..GGG."
      ".GG..GG....."
      "GG....GG....")
    *mettaur-palette*))

(define (make-mettaur pos hp)
  (doc 'type '(-> Pos Int Mettaur))
  (doc 'description "Create a Mettaur virus")
  (doc 'export #t)
  (list 'mettaur pos mettaur-sprite-idle
        `((hp . ,hp)
          (max-hp . ,hp)
          (team . enemy)
          (solid . #t)
          (state . idle)
          (action-timer . ,(+ 1000 (random 1000)))  ; Random delay
          (guard-timer . 0))))

(define (mettaur-state m)
  (entity-get m 'state 'idle))

(define (mettaur-action-timer m)
  (entity-get m 'action-timer 0))

(define (mettaur-guard-timer m)
  (entity-get m 'guard-timer 0))

;;; ============================================================
;;; Mettaur Protocol Implementations
;;; ============================================================

(implement-protocol! 'entity-pos 'mettaur
  (lambda (m) (cadr m)))

(implement-protocol! 'entity-set-pos 'mettaur
  (lambda (m pos) (list 'mettaur pos (caddr m) (cadddr m))))

(implement-protocol! 'entity-sprite 'mettaur
  (lambda (m)
    (case (mettaur-state m)
      [(guard) mettaur-sprite-guard]
      [(attack) mettaur-sprite-attack]
      [else mettaur-sprite-idle])))

(implement-protocol! 'entity-hp 'mettaur
  (lambda (m) (entity-get m 'hp 0)))

(implement-protocol! 'entity-set-hp 'mettaur
  (lambda (m hp)
    (let* ([props (entity-props m)]
           [new-props (cons (cons 'hp hp)
                            (remp (lambda (p) (eq? (car p) 'hp)) props))])
      (entity-with-props m new-props))))

(implement-protocol! 'entity-team 'mettaur
  (lambda (m) 'enemy))

(implement-protocol! 'entity-solid? 'mettaur
  (lambda (m) #t))

(define (mettaur-with-props m new-props)
  (list 'mettaur (cadr m) (caddr m) new-props))

(define (mettaur-set-prop m prop value)
  (let* ([props (entity-props m)]
         [new-props (cons (cons prop value)
                          (remp (lambda (p) (eq? (car p) prop)) props))])
    (mettaur-with-props m new-props)))

(implement-protocol! 'entity-update 'mettaur
  (lambda (m dt world)
    (let* ([state (mettaur-state m)]
           [action-timer (mettaur-action-timer m)]
           [guard-timer (mettaur-guard-timer m)])
      (case state
        [(idle)
         ;; Count down to next action
         (let ([new-timer (- action-timer dt)])
           (if (<= new-timer 0)
               ;; Start guard phase before attack
               (mettaur-set-prop
                (mettaur-set-prop m 'state 'guard)
                'guard-timer 500)
               (mettaur-set-prop m 'action-timer new-timer)))]
        [(guard)
         ;; Guard phase - invincible
         (let ([new-timer (- guard-timer dt)])
           (if (<= new-timer 0)
               ;; Attack!
               (mettaur-set-prop m 'state 'attack)
               (mettaur-set-prop m 'guard-timer new-timer)))]
        [(attack)
         ;; After attack, return to idle
         (mettaur-set-prop
          (mettaur-set-prop m 'state 'idle)
          'action-timer (+ 1500 (random 1000)))]
        [else m]))))

(implement-protocol! 'entity-on-hit 'mettaur
  (lambda (m damage source)
    ;; Guarding blocks damage
    (if (eq? (mettaur-state m) 'guard)
        m  ; No damage while guarding
        (let ([hp (entity-hp m)])
          (entity-set-hp m (max 0 (- hp damage)))))))

;;; ============================================================
;;; Canodumb Virus (Stationary cannon)
;;; ============================================================

(doc 'section 'canodumb)

(define *canodumb-palette*
  `((#\. . ,color-default)
    (#\R . ,(rgb 255 50 50))         ; Red body
    (#\r . ,(rgb 180 30 30))         ; Dark red
    (#\W . ,(rgb 255 255 255))
    (#\# . ,(rgb 0 0 0))
    (#\G . ,(rgb 80 80 80))))        ; Gun barrel

(define canodumb-sprite
  (define-sprite-from-strings 8 8
    '("..RRRR.."
      ".RRRRRR."
      "RRRRRRGG"
      "RR#R#RGG"
      "RRRRRRGG"
      ".RRRRRr."
      "..rrrr.."
      "..RRRR..")
    *canodumb-palette*))

(define (make-canodumb pos hp)
  (doc 'type '(-> Pos Int Canodumb))
  (doc 'description "Create a Canodumb virus (stationary turret)")
  (doc 'export #t)
  (list 'canodumb pos canodumb-sprite
        `((hp . ,hp)
          (max-hp . ,hp)
          (team . enemy)
          (solid . #t)
          (fire-timer . ,(+ 500 (random 500))))))

(implement-protocol! 'entity-pos 'canodumb
  (lambda (c) (cadr c)))

(implement-protocol! 'entity-set-pos 'canodumb
  (lambda (c pos) (list 'canodumb pos (caddr c) (cadddr c))))

(implement-protocol! 'entity-sprite 'canodumb
  (lambda (c) canodumb-sprite))

(implement-protocol! 'entity-hp 'canodumb
  (lambda (c) (entity-get c 'hp 0)))

(implement-protocol! 'entity-set-hp 'canodumb
  (lambda (c hp)
    (let* ([props (entity-props c)]
           [new-props (cons (cons 'hp hp)
                            (remp (lambda (p) (eq? (car p) 'hp)) props))])
      (list 'canodumb (cadr c) (caddr c) new-props))))

(implement-protocol! 'entity-team 'canodumb
  (lambda (c) 'enemy))

(implement-protocol! 'entity-solid? 'canodumb
  (lambda (c) #t))

(implement-protocol! 'entity-update 'canodumb
  (lambda (c dt world)
    ;; Canodumb just fires periodically
    (let* ([timer (entity-get c 'fire-timer 0)]
           [new-timer (- timer dt)])
      (if (<= new-timer 0)
          ;; Fire and reset timer (actual projectile handled by battle system)
          (let* ([props (entity-props c)]
                 [new-props (cons (cons 'fire-timer (+ 800 (random 400)))
                                  (cons (cons 'fired #t)
                                        (remp (lambda (p)
                                                (memq (car p) '(fire-timer fired)))
                                              props)))])
            (list 'canodumb (cadr c) (caddr c) new-props))
          ;; Just update timer
          (let* ([props (entity-props c)]
                 [new-props (cons (cons 'fire-timer new-timer)
                                  (remp (lambda (p) (eq? (car p) 'fire-timer)) props))])
            (list 'canodumb (cadr c) (caddr c) new-props))))))

(implement-protocol! 'entity-on-hit 'canodumb
  (lambda (c damage source)
    (let ([hp (entity-hp c)])
      (entity-set-hp c (max 0 (- hp damage))))))

(display "mmbn/virus.ss loaded.\n")
(display "  (make-mettaur pos hp)    — Yellow hardhat virus\n")
(display "  (make-canodumb pos hp)   — Stationary cannon virus\n")
