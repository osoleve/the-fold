;;; test-regex-extensions.ss — Tests for regex extensions
;;;
;;; Tests: quantifier ranges {n,m}, anchors ^/$, lookahead (?=...)/(?!...)

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/fp/parsing/regex.ss")

(test-group "Regex Extensions"

  ;;; ============================================================
  ;;; Quantifier Ranges
  ;;; ============================================================

  (test-group "Quantifier Ranges - Parsing"

    (define-test "parse {3} exact"
      (let ([result (regex-parse "a{3}")])
        (assert-true (right? result))
        (let ([ast (from-right result)])
          (assert-true (regex-repeat? ast))
          (assert-equal 3 (regex-repeat-min ast))
          (assert-equal 3 (regex-repeat-max ast)))))

    (define-test "parse {2,4} range"
      (let ([result (regex-parse "a{2,4}")])
        (assert-true (right? result))
        (let ([ast (from-right result)])
          (assert-true (regex-repeat? ast))
          (assert-equal 2 (regex-repeat-min ast))
          (assert-equal 4 (regex-repeat-max ast)))))

    (define-test "parse {2,} unbounded"
      (let ([result (regex-parse "a{2,}")])
        (assert-true (right? result))
        (let ([ast (from-right result)])
          (assert-true (regex-repeat? ast))
          (assert-equal 2 (regex-repeat-min ast))
          (assert-equal #f (regex-repeat-max ast)))))

    (define-test "parse {,4} from zero"
      (let ([result (regex-parse "a{,4}")])
        (assert-true (right? result))
        (let ([ast (from-right result)])
          (assert-true (regex-repeat? ast))
          (assert-equal 0 (regex-repeat-min ast))
          (assert-equal 4 (regex-repeat-max ast)))))

    (define-test "parse {} invalid"
      (let ([result (regex-parse "a{}")])
        (assert-true (left? result)))))

  (test-group "Quantifier Ranges - Matching"

    (define-test "a{3} matches aaa"
      (assert-true (regex-accepts? "a{3}" "aaa")))

    (define-test "a{3} rejects aa"
      (assert-false (regex-accepts? "a{3}" "aa")))

    (define-test "a{3} rejects aaaa"
      (assert-false (regex-accepts? "a{3}" "aaaa")))

    (define-test "a{2,4} matches aa"
      (assert-true (regex-accepts? "a{2,4}" "aa")))

    (define-test "a{2,4} matches aaa"
      (assert-true (regex-accepts? "a{2,4}" "aaa")))

    (define-test "a{2,4} matches aaaa"
      (assert-true (regex-accepts? "a{2,4}" "aaaa")))

    (define-test "a{2,4} rejects a"
      (assert-false (regex-accepts? "a{2,4}" "a")))

    (define-test "a{2,4} rejects aaaaa"
      (assert-false (regex-accepts? "a{2,4}" "aaaaa")))

    (define-test "a{2,} matches aa"
      (assert-true (regex-accepts? "a{2,}" "aa")))

    (define-test "a{2,} matches aaaaa"
      (assert-true (regex-accepts? "a{2,}" "aaaaa")))

    (define-test "a{2,} rejects a"
      (assert-false (regex-accepts? "a{2,}" "a")))

    (define-test "a{,2} matches empty"
      (assert-true (regex-accepts? "a{,2}" "")))

    (define-test "a{,2} matches a"
      (assert-true (regex-accepts? "a{,2}" "a")))

    (define-test "a{,2} matches aa"
      (assert-true (regex-accepts? "a{,2}" "aa")))

    (define-test "a{,2} rejects aaa"
      (assert-false (regex-accepts? "a{,2}" "aaa")))

    (define-test "a{0,0} matches empty only"
      (assert-true (regex-accepts? "a{0,0}" ""))
      (assert-false (regex-accepts? "a{0,0}" "a"))))

  ;;; ============================================================
  ;;; Anchors
  ;;; ============================================================

  (test-group "Anchors - Parsing"

    (define-test "parse ^ start anchor"
      (let ([result (regex-parse "^")])
        (assert-true (right? result))
        (let ([ast (from-right result)])
          (assert-true (regex-anchor? ast))
          (assert-equal 'start (regex-anchor-type ast)))))

    (define-test "parse $ end anchor"
      (let ([result (regex-parse "$")])
        (assert-true (right? result))
        (let ([ast (from-right result)])
          (assert-true (regex-anchor? ast))
          (assert-equal 'end (regex-anchor-type ast)))))

    (define-test "parse ^foo$"
      (let ([result (regex-parse "^foo$")])
        (assert-true (right? result)))))

  (test-group "Anchors - Matching"

    (define-test "^foo matches foo at start"
      (assert-true (regex-accepts? "^foo" "foo")))

    (define-test "foo$ matches foo at end"
      (assert-true (regex-accepts? "foo$" "foo")))

    (define-test "^foo$ matches exactly foo"
      (assert-true (regex-accepts? "^foo$" "foo")))

    (define-test "^$ matches empty string"
      (assert-true (regex-accepts? "^$" "")))

    (define-test "^$ rejects non-empty"
      (assert-false (regex-accepts? "^$" "x")))

    (define-test "^ alone matches empty"
      (assert-true (regex-accepts? "^" "")))

    (define-test "$ alone matches empty"
      (assert-true (regex-accepts? "$" ""))))

  ;;; ============================================================
  ;;; Lookahead
  ;;; ============================================================

  (test-group "Lookahead - Parsing"

    (define-test "parse (?=foo) positive lookahead"
      (let ([result (regex-parse "(?=foo)")])
        (assert-true (right? result))
        (let ([ast (from-right result)])
          (assert-true (regex-lookahead? ast))
          (assert-true (regex-lookahead-positive? ast)))))

    (define-test "parse (?!foo) negative lookahead"
      (let ([result (regex-parse "(?!foo)")])
        (assert-true (right? result))
        (let ([ast (from-right result)])
          (assert-true (regex-lookahead? ast))
          (assert-false (regex-lookahead-positive? ast)))))

    (define-test "parse a(?=b) lookahead in sequence"
      (let ([result (regex-parse "a(?=b)")])
        (assert-true (right? result))))

    (define-test "regular group still works"
      (let ([result (regex-parse "(abc)")])
        (assert-true (right? result))
        (let ([ast (from-right result)])
          (assert-true (regex-group? ast))))))

  (test-group "Lookahead - Matching"

    (define-test "a(?=b) matches a followed by b"
      ;; This matches 'a' only when followed by 'b', but doesn't consume 'b'
      ;; So "ab" should match because 'a' is followed by 'b'
      ;; But the full match is just 'a', so we need "a" followed by unconsumed "b"
      ;; Actually for full-string matching, a(?=b) should match "a" only if the string is "a" followed by "b"
      ;; But since we're doing full-string match, a(?=b) against "ab" should fail
      ;; because after matching 'a' and checking lookahead, we have 'b' left unmatched
      ;; Let me think... a(?=b) means: match 'a', then assert 'b' follows (without consuming)
      ;; For full string "ab": match 'a' at pos 0, lookahead at pos 1 sees 'b' - succeeds
      ;; But then we're at pos 1 with 'b' remaining and no more pattern
      ;; So full-string match fails because input isn't fully consumed
      ;; For "a(?=b)b" against "ab": match 'a', lookahead sees 'b', then match 'b' - full match!
      (assert-true (regex-accepts? "a(?=b)b" "ab")))

    (define-test "a(?=b)c rejects ab"
      ;; a(?=b)c means: 'a', then assert 'b' follows, then match 'c'
      ;; Against "ab": 'a' matches, lookahead sees 'b' succeeds, but then 'c' fails against 'b'
      (assert-false (regex-accepts? "a(?=b)c" "ab")))

    (define-test "a(?!b) rejects when followed by b"
      ;; a(?!b) against "ab": 'a' matches, negative lookahead checks if 'b' doesn't follow
      ;; 'b' does follow, so negative lookahead fails
      (assert-false (regex-accepts? "a(?!b)." "ab")))

    (define-test "a(?!b) accepts when followed by c"
      ;; a(?!b) against "ac": 'a' matches, negative lookahead checks if 'b' doesn't follow
      ;; 'c' follows (not 'b'), so negative lookahead succeeds
      (assert-true (regex-accepts? "a(?!b)." "ac")))

    (define-test "(?=a)a matches a"
      ;; Lookahead at start: assert 'a', then match 'a'
      (assert-true (regex-accepts? "(?=a)a" "a")))

    (define-test "(?!b)a matches a"
      ;; Negative lookahead at start: assert not 'b', then match 'a'
      (assert-true (regex-accepts? "(?!b)a" "a")))

    (define-test "(?!b)a rejects b"
      ;; Negative lookahead at start: assert not 'b' - fails because input is 'b'
      (assert-false (regex-accepts? "(?!b)a" "b"))))

  ;;; ============================================================
  ;;; Combined Features
  ;;; ============================================================

  (test-group "Combined Features"

    (define-test "^a{3}$ matches aaa exactly"
      (assert-true (regex-accepts? "^a{3}$" "aaa")))

    (define-test "^a{3}$ rejects aa"
      (assert-false (regex-accepts? "^a{3}$" "aa")))

    (define-test "^(?=.{3}$)a+ matches aaa"
      ;; Assert string is exactly 3 chars, then match one or more 'a's
      (assert-true (regex-accepts? "^(?=.{3}$)a+$" "aaa")))

    (define-test "nested lookahead (?=a(?!b))"
      ;; Lookahead containing negative lookahead
      (let ([result (regex-parse "(?=a(?!b))")])
        (assert-true (right? result))))))

(run-all-tests)
