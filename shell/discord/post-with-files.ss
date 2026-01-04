;;; shell/discord/post-with-files.ss — Post to Discord with file attachments
;;;
;;; Helper to create Discord outbox messages with file attachments.
;;; Uses the JSON outbox system for bridge.js to pick up.
;;;
;;; Usage:
;;;   (load "shell/discord/post-with-files.ss")
;;;   (discord-post-files 'art "Amazing animation!" "Check this out"
;;;                       '("user/creations/spinning-torus.gif"))

;;; ============================================================
;;; Dependencies
;;; ============================================================

(load "core/base/prelude.ss")

;;; ============================================================
;;; JSON Serialization (minimal)
;;; ============================================================

(define (escape-json-string s)
  "Escape special characters for JSON"
  (let loop ([chars (string->list s)]
             [result '()])
       (if (null? chars)
           (list->string (reverse result))
           (let ([c (car chars)])
                (cond
                 [(char=? c #\") (loop (cdr chars) (append '(#\" #\\) result))]
                 [(char=? c #\\) (loop (cdr chars) (append '(#\\ #\\) result))]
                 [(char=? c #\newline) (loop (cdr chars) (append '(#\n #\\) result))]
                 [(char=? c #\return) (loop (cdr chars) (append '(#\r #\\) result))]
                 [(char=? c #\tab) (loop (cdr chars) (append '(#\t #\\) result))]
                 [else (loop (cdr chars) (cons c result))])))))

(define (value->json v)
  "Convert Scheme value to JSON string"
  (cond
   [(string? v) (string-append "\"" (escape-json-string v) "\"")]
   [(symbol? v) (string-append "\"" (symbol->string v) "\"")]
   [(number? v) (number->string v)]
   [(boolean? v) (if v "true" "false")]
   [(null? v) "null"]
   [(list? v)
    ;; Assume list of strings for arrays
    (string-append "["
                   (apply string-append
                          (let loop ([items v] [first? #t])
                               (if (null? items)
                                   '()
                                   (let ([prefix (if first? "" ",")])
                                        (cons (string-append prefix (value->json (car items)))
                                              (loop (cdr items) #f))))))
                   "]")]
   [else "null"]))

;;; ============================================================
;;; Outbox Message Creation
;;; ============================================================

(define (make-discord-outbox-path)
  "Generate unique outbox file path"
  (let ([timestamp (time-second (current-time))])
       (string-append ".fold-repl/discord-outbox/"
                      (number->string timestamp)
                      "-"
                      (number->string (random 1000000))
                      ".json")))

(define (write-discord-outbox! channel title body author tier attachments)
  "Write a Discord outbox message with attachments"
  (let* ([json-parts (list
                      "{"
                      "\"channel\":" (value->json (symbol->string channel)) ","
                      "\"title\":" (value->json title) ","
                      "\"body\":" (value->json body) ","
                      "\"author\":" (value->json author) ","
                      "\"tier\":" (value->json tier) ","
                      "\"attachments\":" (value->json attachments)
                      "}")]
         [json (apply string-append json-parts)]
         [path (make-discord-outbox-path)])
        ;; Ensure outbox directory exists
        (system "mkdir -p .fold-repl/discord-outbox")
        ;; Write JSON file
        (call-with-output-file path
                               (lambda (port)
                                       (put-string port json)))
        (display "📤 Queued for Discord: ")
        (display path)
        (newline)
        path))

;;; ============================================================
;;; User-Facing API
;;; ============================================================

(define (discord-post-files channel title body files)
  "Post to Discord with file attachments

  channel: Symbol - Discord channel (art, engineering, etc.)
  title: String - Post title
  body: String - Post body text
  files: List of Strings - File paths (relative to project root)

  Example:
    (discord-post-files 'art
                       \"Spinning Torus\"
                       \"Ray-marched SDF geometry in ASCII, exported to GIF!\"
                       '(\"user/creations/spinning-torus.gif\"))"
  (let ([author (getenv "USER")]  ; Or get from session context
        [tier "builder"])         ; Default tier
       (write-discord-outbox! channel title body author tier files)))

(define (discord-chat-files channel message files)
  "Post a chat message with file attachments (no title/embed)"
  (let ([author (getenv "USER")]
        [tier "builder"])
       (write-discord-outbox! channel "" message author tier files)))

;;; ============================================================
;;; Quick Helpers
;;; ============================================================

(define (discord-gif channel title body gif-path)
  "Post a single GIF to Discord"
  (discord-post-files channel title body (list gif-path)))

(define (discord-mp4 channel title body mp4-path)
  "Post a single MP4 to Discord"
  (discord-post-files channel title body (list mp4-path)))

;;; ============================================================
;;; Demo
;;; ============================================================

(define (post-files-demo)
  (display "\n")
  (display "============================================================\n")
  (display "  DISCORD FILE POSTING\n")
  (display "============================================================\n")
  (display "  Post files to Discord via the outbox bridge\n")
  (display "============================================================\n\n")
  
  (display "Usage:\n")
  (display "  (discord-post-files 'art \"Title\" \"Body\" '(\"path/to/file.gif\"))\n")
  (display "  (discord-chat-files 'art \"Message\" '(\"file1.png\" \"file2.png\"))\n")
  (display "\n")
  (display "Quick helpers:\n")
  (display "  (discord-gif 'art \"Title\" \"Body\" \"output.gif\")\n")
  (display "  (discord-mp4 'art \"Title\" \"Body\" \"output.mp4\")\n")
  (display "\n")
  (display "File paths are relative to project root.\n")
  (display "============================================================\n"))

(post-files-demo)
