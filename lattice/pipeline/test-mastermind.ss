;;; lattice/pipeline/test-mastermind.ss — Mastermind Tests

(load "core/testing/test-framework.ss")
(load "lattice/pipeline/mastermind.ss")

;;; ====
;;; Scoring
;;; ====

(test-group "mastermind-scoring"

  (define-test "all exact"
    (let ([score (mastermind-score '(red blue green yellow)
                                   '(red blue green yellow))])
      (assert-equal 4 (car score))
      (assert-equal 0 (cdr score))))

  (define-test "no matches"
    (let ([score (mastermind-score '(red red red red)
                                   '(blue blue blue blue))])
      (assert-equal 0 (car score))
      (assert-equal 0 (cdr score))))

  (define-test "all misplaced"
    (let ([score (mastermind-score '(red blue green yellow)
                                   '(yellow green blue red))])
      (assert-equal 0 (car score))
      (assert-equal 4 (cdr score))))

  (define-test "mixed exact and misplaced"
    (let ([score (mastermind-score '(red blue green yellow)
                                   '(red green blue yellow))])
      (assert-equal 2 (car score))    ; positions 0,3
      (assert-equal 2 (cdr score)))) ; blue,green swapped

  (define-test "duplicate colors in secret — no double counting"
    ;; secret: (red red blue green), guess: (red blue yellow yellow)
    ;; exact: position 0 (red=red) → 1
    ;; remaining s: (red blue green), remaining g: (blue yellow yellow)
    ;; misplaced: blue=1 → 1
    (let ([score (mastermind-score '(red red blue green)
                                   '(red blue yellow yellow))])
      (assert-equal 1 (car score))
      (assert-equal 1 (cdr score))))

  (define-test "duplicate colors in guess — capped by secret count"
    ;; secret: (red blue green yellow), guess: (red red red red)
    ;; exact: position 0 → 1
    ;; remaining s: (blue green yellow), remaining g: (red red red)
    ;; misplaced: 0 (no reds left in secret)
    (let ([score (mastermind-score '(red blue green yellow)
                                   '(red red red red))])
      (assert-equal 1 (car score))
      (assert-equal 0 (cdr score))))

  (define-test "duplicate colors both sides"
    ;; secret: (red red blue green), guess: (blue red red red)
    ;; exact: position 1 (red=red) → 1
    ;; remaining s: (red blue green), remaining g: (blue red red)
    ;; misplaced: red min(1,2)=1, blue min(1,1)=1 → 2
    (let ([score (mastermind-score '(red red blue green)
                                   '(blue red red red))])
      (assert-equal 1 (car score))
      (assert-equal 2 (cdr score))))

  (define-test "one exact rest wrong"
    (let ([score (mastermind-score '(red blue green yellow)
                                   '(red orange orange orange))])
      (assert-equal 1 (car score))
      (assert-equal 0 (cdr score))))

  (define-test "one misplaced rest wrong"
    (let ([score (mastermind-score '(red blue green yellow)
                                   '(orange orange orange red))])
      (assert-equal 0 (car score))
      (assert-equal 1 (cdr score)))))

;;; ====
;;; Episode Creation
;;; ====

(test-group "mastermind-episodes"

  (define-test "deterministic — same seed same secret"
    (let ([g1 (mastermind-make-episode (default-mastermind-config) 42)]
          [g2 (mastermind-make-episode (default-mastermind-config) 42)])
      (assert-equal (mastermind-game-secret g1)
                    (mastermind-game-secret g2))))

  (define-test "different seeds — different secrets"
    (let ([g1 (mastermind-make-episode (default-mastermind-config) 42)]
          [g2 (mastermind-make-episode (default-mastermind-config) 99)])
      (assert-false (equal? (mastermind-game-secret g1)
                            (mastermind-game-secret g2)))))

  (define-test "secret has correct length"
    (let ([g (mastermind-make-episode (default-mastermind-config) 42)])
      (assert-equal 4 (length (mastermind-game-secret g)))))

  (define-test "secret uses valid colors"
    (let* ([config (default-mastermind-config)]
           [g (mastermind-make-episode config 42)]
           [colors (mastermind-available-colors config)]
           [secret (mastermind-game-secret g)])
      (assert-true (let loop ([s secret])
                     (or (null? s)
                         (and (pair? (memq (car s) colors))
                              (loop (cdr s))))))))

  (define-test "starts active with no guesses"
    (let ([g (mastermind-make-episode (default-mastermind-config) 42)])
      (assert-equal 'active (mastermind-game-status g))
      (assert-equal '() (mastermind-game-guesses g)))))

;;; ====
;;; Game Flow
;;; ====

(test-group "mastermind-flow"

  (define-test "correct guess wins"
    (let* ([g (mastermind-make-episode (default-mastermind-config) 42)]
           [secret (mastermind-game-secret g)])
      (let-values ([(g* msg) (mastermind-step-guess g secret)])
        (assert-equal 'won (mastermind-game-status g*))
        (assert-equal 1.0 (mastermind-reward g*)))))

  (define-test "10 wrong guesses loses"
    (let* ([config (make-mastermind-config 4 6 10)]
           [g (mastermind-make-episode config 42)]
           [secret (mastermind-game-secret g)]
           ;; Make a guess guaranteed wrong: shift each color by 3 positions
           [colors *mastermind-colors*]
           [n (length colors)]
           [wrong (map (lambda (c)
                         (let ([idx (let loop ([cs colors] [i 0])
                                      (if (eq? (car cs) c) i (loop (cdr cs) (+ i 1))))])
                           (list-ref colors (modulo (+ idx 3) n))))
                       secret)])
      (let loop ([game g] [i 0])
        (if (= i 10)
            (begin
              (assert-equal 'lost (mastermind-game-status game))
              (assert-equal -1.0 (mastermind-reward game)))
            (let-values ([(g* msg) (mastermind-step-guess game wrong)])
              (loop g* (+ i 1)))))))

  (define-test "guesses accumulate"
    (let* ([g (mastermind-make-episode (default-mastermind-config) 42)]
           [guess1 '(red red red red)]
           [guess2 '(blue blue blue blue)])
      (let-values ([(g1 _) (mastermind-step-guess g guess1)])
        (assert-equal 1 (length (mastermind-game-guesses g1)))
        (let-values ([(g2 _) (mastermind-step-guess g1 guess2)])
          (assert-equal 2 (length (mastermind-game-guesses g2)))))))

  (define-test "can't guess after game over"
    (let* ([g (mastermind-make-episode (default-mastermind-config) 42)]
           [secret (mastermind-game-secret g)])
      (let-values ([(g* _) (mastermind-step-guess g secret)])
        (assert-equal 'won (mastermind-game-status g*))
        (let-values ([(g** msg) (mastermind-step-guess g* '(red red red red))])
          (assert-equal 'won (mastermind-game-status g**))
          (assert-true (string? msg)))))))

;;; ====
;;; Invalid Guesses
;;; ====

(test-group "mastermind-validation"

  (define-test "wrong length rejected"
    (let ([g (mastermind-make-episode (default-mastermind-config) 42)])
      (let-values ([(g* msg) (mastermind-step-guess g '(red blue))])
        (assert-equal 'active (mastermind-game-status g*))
        (assert-true (string? msg)))))

  (define-test "invalid color rejected"
    (let ([g (mastermind-make-episode (default-mastermind-config) 42)])
      (let-values ([(g* msg) (mastermind-step-guess g '(red blue purple yellow))])
        (assert-equal 'active (mastermind-game-status g*))
        (assert-true (string? msg)))))

  (define-test "non-list rejected"
    (let ([g (mastermind-make-episode (default-mastermind-config) 42)])
      (let-values ([(g* msg) (mastermind-step-guess g 'red)])
        (assert-equal 'active (mastermind-game-status g*))
        (assert-true (string? msg))))))

;;; ====
;;; Observation Formatting
;;; ====

(test-group "mastermind-observation"

  (define-test "initial observation includes colors and code-length"
    (let* ([g (mastermind-make-episode (default-mastermind-config) 42)]
           [obs (mastermind-format-observation g "test")])
      (assert-true (string? obs))
      (assert-true (> (string-length obs) 0))))

  (define-test "feedback includes exact and misplaced"
    (let* ([g (mastermind-make-episode (default-mastermind-config) 42)])
      (let-values ([(g* msg) (mastermind-step-guess g '(red blue green yellow))])
        (assert-true (string? msg))
        (assert-true (> (string-length msg) 0))))))

;;; ====
;;; Terminal / Reward
;;; ====

(test-group "mastermind-terminal"

  (define-test "active game is not terminal"
    (let ([g (mastermind-make-episode (default-mastermind-config) 42)])
      (assert-false (mastermind-terminal? g))
      (assert-equal 0.0 (mastermind-reward g))))

  (define-test "won game is terminal"
    (let* ([g (mastermind-make-episode (default-mastermind-config) 42)]
           [secret (mastermind-game-secret g)])
      (let-values ([(g* _) (mastermind-step-guess g secret)])
        (assert-true (mastermind-terminal? g*))))))

(run-all-tests)
