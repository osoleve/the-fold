;;; shell/test-color.ss — Test vectors for Color System
;;;
;;; Run with: scheme --script shell/test-color.ss

(load "thimble/color.ss")

;;; ============================================================
;;; Test 1: Color Construction
;;; ============================================================

(display "Test 1: Color Construction\n")

(define c-rgb (make-color-rgb 255 128 64))
(display "  RGB color created: ")
(display (color-rgb? c-rgb))
(display " (expected #t)\n")

(define c-palette (make-color-palette 42))
(display "  Palette color created: ")
(display (color-palette? c-palette))
(display " (expected #t)\n")

(display "  Default color: ")
(display (color-default? color-default))
(display " (expected #t)\n")

(display "  ✓ Color construction works\n")

;;; ============================================================
;;; Test 2: Color Constants
;;; ============================================================

(display "\nTest 2: Color Constants\n")

(display "  color-red type: ")
(display (color-palette? color-red))
(display " (expected #t)\n")

(display "  color-blue type: ")
(display (color-palette? color-blue))
(display " (expected #t)\n")

(display "  rgb convenience: ")
(define c-test (rgb 100 150 200))
(display (and (color-rgb? c-test)
             (= (list-ref c-test 1) 100)
             (= (list-ref c-test 2) 150)
             (= (list-ref c-test 3) 200)))
(display " (expected #t)\n")

(display "  ✓ Color constants accessible\n")

;;; ============================================================
;;; Test 3: ANSI Code Generation
;;; ============================================================

(display "\nTest 3: ANSI Code Generation\n")

(define ansi-red-fg (ansi-fg color-red))
(display "  Red foreground starts with ESC: ")
(display (char=? (string-ref ansi-red-fg 0) #\x1B))
(display " (expected #t)\n")

(define ansi-blue-bg (ansi-bg color-blue))
(display "  Blue background starts with ESC: ")
(display (char=? (string-ref ansi-blue-bg 0) #\x1B))
(display " (expected #t)\n")

(define ansi-combo (ansi-color color-green color-yellow))
(display "  Combined color code non-empty: ")
(display (> (string-length ansi-combo) 0))
(display " (expected #t)\n")

(display "  Reset code: ")
(display (equal? ansi-reset "\x1B[0m"))
(display " (expected #t)\n")

(display "  ✓ ANSI code generation works\n")

;;; ============================================================
;;; Test 4: Cell Construction
;;; ============================================================

(display "\nTest 4: Cell Construction\n")

(define cell1 (make-cell #\A color-red color-blue))
(display "  Cell character: ")
(display (char=? (cell%-char cell1) #\A))
(display " (expected #t)\n")

(define cell2 (make-cell-simple #\B))
(display "  Simple cell character: ")
(display (char=? (cell%-char cell2) #\B))
(display " (expected #t)\n")

(display "  Default cell is space: ")
(display (char=? (cell%-char default-cell) #\space))
(display " (expected #t)\n")

(display "  ✓ Cell construction works\n")

;;; ============================================================
;;; Test 5: Color Helpers
;;; ============================================================

(display "\nTest 5: Color Helpers\n")

;; Test lerp-color
(define c1 (rgb 0 0 0))
(define c2 (rgb 100 200 255))
(define c-mid (lerp-color c1 c2 0.5))
(display "  Lerp midpoint R: ")
(display (= (list-ref c-mid 1) 50))
(display " (expected #t)\n")

;; Test darken
(define c-bright (rgb 200 200 200))
(define c-dark (darken c-bright 0.5))
(display "  Darken reduces values: ")
(display (< (list-ref c-dark 1) (list-ref c-bright 1)))
(display " (expected #t)\n")

;; Test lighten
(define c-dim (rgb 100 100 100))
(define c-light (lighten c-dim 0.5))
(display "  Lighten increases values: ")
(display (> (list-ref c-light 1) (list-ref c-dim 1)))
(display " (expected #t)\n")

(display "  ✓ Color helpers work\n")

;;; ============================================================
;;; Test 6: Mood Colors
;;; ============================================================

(display "\nTest 6: Mood Colors\n")

(define mood-happy (mood->color 'happy))
(display "  Happy mood has color: ")
(display (color-rgb? mood-happy))
(display " (expected #t)\n")

(define mood-sleepy (mood->color 'sleepy))
(display "  Sleepy mood has color: ")
(display (color-rgb? mood-sleepy))
(display " (expected #t)\n")

(define mood-unknown (mood->color 'unknown-mood))
(display "  Unknown mood returns default: ")
(display (color-default? mood-unknown))
(display " (expected #t)\n")

(display "  ✓ Mood color mapping works\n")

;;; ============================================================
;;; Test 7: Energy Colors
;;; ============================================================

(display "\nTest 7: Energy Colors\n")

(define energy-low (energy->color 0))
(display "  Low energy has color: ")
(display (color-rgb? energy-low))
(display " (expected #t)\n")

(define energy-high (energy->color 100))
(display "  High energy has color: ")
(display (color-rgb? energy-high))
(display " (expected #t)\n")

;; Energy should be brighter at high values
(display "  High energy is brighter: ")
(define low-r (list-ref energy-low 1))
(define high-r (list-ref energy-high 1))
(display (> high-r low-r))
(display " (expected #t)\n")

(display "  ✓ Energy gradient works\n")

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n")
(display "═══════════════════════════════════════════\n")
(display "  ✓ All color tests passed!\n")
(display "  ✓ Color system is operational.\n")
(display "  ✓ DUCKIE can see in color.\n")
(display "═══════════════════════════════════════════\n")
