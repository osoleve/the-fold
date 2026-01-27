;;; user/mmbn-sdk/entities/navi.ss — Player Navi Entity
;;; @module mmbn/navi
;;; @requires entity, sprite-designer
;;; Load via: (load "user/mmbn-sdk/mmbn.ss")

;; Dependencies loaded by mmbn.ss

(doc 'module 'mmbn/navi)
(doc 'description "Player Navi (NetNavi) entity. The player-controlled character
that battles viruses and navigates the Net.")
(doc 'layer 'user)
(doc 'purity 'partial)

;;; ============================================================
;;; Navi Sprites
;;; ============================================================

(doc 'section 'sprites)

(define *navi-palette*
  `((#\. . ,color-default)           ; Transparent
    (#\B . ,(rgb 0 100 255))         ; Blue body
    (#\b . ,(rgb 0 60 180))          ; Dark blue
    (#\W . ,(rgb 255 255 255))       ; White
    (#\Y . ,(rgb 255 255 0))         ; Yellow (emblem)
    (#\S . ,(rgb 255 200 150))       ; Skin
    (#\# . ,(rgb 0 0 0))             ; Black
    (#\C . ,(rgb 0 200 255))))       ; Cyan accent

(define navi-sprite-idle
  (define-sprite-from-strings 8 12
    '("...BB..."
      "..BWWB.."
      ".BWSSYB."
      ".BS##SB."
      "..BSSB.."
      "...BB..."
      "..BBBB.."
      ".BCBBCB."
      ".BBBBBB."
      "..BBBB.."
      "..B..B.."
      ".BB..BB.")
    *navi-palette*))

(define navi-sprite-attack
  (define-sprite-from-strings 12 12
    '("...BB...WWWW"
      "..BWWB..WWWW"
      ".BWSSYB.WWWW"
      ".BS##SB.WWWW"
      "..BSSB......"
      "...BB......."
      "..BBBB......"
      ".BCBBCB....."
      ".BBBBBB....."
      "..BBBB......"
      "..B..B......"
      ".BB..BB.....")
    *navi-palette*))

;;; ============================================================
;;; Navi Entity
;;; ============================================================

(doc 'section 'navi-entity)

(define (make-navi pos hp)
  (doc 'type '(-> Pos Int Navi))
  (doc 'description "Create a player Navi")
  (doc 'export #t)
  (list 'navi pos navi-sprite-idle
        `((hp . ,hp)
          (max-hp . ,hp)
          (team . player)
          (solid . #t)
          (state . idle)
          (attack-timer . 0))))

(define (navi-state n)
  (entity-get n 'state 'idle))

(define (navi-attack-timer n)
  (entity-get n 'attack-timer 0))

(define (navi-with-state n state)
  (let* ([props (entity-props n)]
         [new-props (cons (cons 'state state)
                          (remp (lambda (p) (eq? (car p) 'state)) props))])
    (entity-with-props n new-props)))

(define (navi-with-timer n timer)
  (let* ([props (entity-props n)]
         [new-props (cons (cons 'attack-timer timer)
                          (remp (lambda (p) (eq? (car p) 'attack-timer)) props))])
    (entity-with-props n new-props)))

;;; ============================================================
;;; Navi Protocol Implementations
;;; ============================================================

(doc 'section 'protocol-impl)

(implement-protocol! 'entity-pos 'navi
  (lambda (n) (cadr n)))

(implement-protocol! 'entity-set-pos 'navi
  (lambda (n pos) (list 'navi pos (caddr n) (cadddr n))))

(implement-protocol! 'entity-sprite 'navi
  (lambda (n)
    (case (navi-state n)
      [(attack) navi-sprite-attack]
      [else navi-sprite-idle])))

(implement-protocol! 'entity-hp 'navi
  (lambda (n) (entity-get n 'hp 0)))

(implement-protocol! 'entity-set-hp 'navi
  (lambda (n hp)
    (let* ([props (entity-props n)]
           [new-props (cons (cons 'hp hp)
                            (remp (lambda (p) (eq? (car p) 'hp)) props))])
      (entity-with-props n new-props))))

(implement-protocol! 'entity-team 'navi
  (lambda (n) 'player))

(implement-protocol! 'entity-solid? 'navi
  (lambda (n) #t))

(implement-protocol! 'entity-update 'navi
  (lambda (n dt world)
    ;; Handle attack animation timing
    (if (eq? (navi-state n) 'attack)
        (let ([timer (- (navi-attack-timer n) dt)])
          (if (<= timer 0)
              (navi-with-state (navi-with-timer n 0) 'idle)
              (navi-with-timer n timer)))
        n)))

(implement-protocol! 'entity-on-hit 'navi
  (lambda (n damage source)
    (let ([hp (entity-hp n)])
      (entity-set-hp n (max 0 (- hp damage))))))

;;; ============================================================
;;; Navi Actions
;;; ============================================================

(doc 'section 'actions)

(define (navi-attack n)
  (doc 'type '(-> Navi Navi))
  (doc 'description "Start attack animation")
  (doc 'export #t)
  (navi-with-state (navi-with-timer n 200) 'attack))  ; 200ms attack duration

(define (navi-can-move? n grid direction)
  (doc 'type '(-> Navi BattleGrid Pos Bool))
  (doc 'description "Check if Navi can move in direction")
  (doc 'export #t)
  (let ([new-pos (pos-add (entity-pos n) direction)])
    (grid-can-move-to? grid new-pos 'player)))

(define (navi-try-move n grid direction)
  (doc 'type '(-> Navi BattleGrid Pos Navi))
  (doc 'description "Move Navi if valid, otherwise return unchanged")
  (doc 'export #t)
  (if (navi-can-move? n grid direction)
      (entity-set-pos n (pos-add (entity-pos n) direction))
      n))

(display "mmbn/navi.ss loaded.\n")
(display "  (make-navi pos hp)        — Create player Navi\n")
(display "  (navi-attack n)           — Start attack\n")
(display "  (navi-try-move n g dir)   — Move if valid\n")
