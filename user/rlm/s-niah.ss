;;; user/rlm/s-niah.ss — Single Needle-in-a-Haystack (S-NIAH)
;;;
;;; Replicating the S-NIAH task from "Recursive Language Models"
;;; (Zhang, Kraska, Khattab — arXiv:2512.24601).
;;;
;;; Generates a large text haystack, hides a needle (secret passphrase),
;;; and runs the RLM harness to find it.
;;;
;;; Run: RLM_INTEGRATION=1 scheme --script user/rlm/s-niah.ss

(unless (getenv "RLM_INTEGRATION")
  (display "Skipping S-NIAH (set RLM_INTEGRATION=1 to enable)\n")
  (exit 0))

(load "boundary/pipeline/rlm-loop.ss")

;;; ====
;;; Filler Corpus
;;; ====
;;;
;;; Varied paragraphs across topics. Each ~200-400 chars.
;;; The model must search through these to find the planted needle.

(define *filler-paragraphs*
  '#("The process of photosynthesis converts carbon dioxide and water into glucose and oxygen using sunlight. Chlorophyll in plant cells absorbs light energy, which drives the light-dependent reactions in the thylakoid membranes. The Calvin cycle then uses the products of these reactions to fix carbon into organic molecules."

     "Medieval European castles evolved from simple wooden motte-and-bailey constructions to sophisticated stone fortifications over several centuries. Concentric castle design, with multiple rings of walls, became common after the Crusades introduced Western Europeans to Byzantine and Islamic military architecture."

     "The preparation of a proper French bechamel sauce begins with a roux: equal parts butter and flour cooked together until the raw flour taste disappears. Whole milk is then added gradually while whisking constantly to prevent lumps. The sauce should simmer for at least twenty minutes to reach the proper consistency."

     "The Mariana Trench in the western Pacific Ocean reaches a depth of approximately 11,034 meters at its deepest point, the Challenger Deep. The water pressure at this depth exceeds 1,000 atmospheres, creating an environment that few organisms can survive in without specialized adaptations."

     "Quantum entanglement occurs when pairs of particles interact in ways that make the quantum state of each particle dependent on the state of the other, regardless of the distance between them. Einstein famously described this phenomenon as spooky action at a distance, questioning the completeness of quantum mechanics."

     "The Fibonacci sequence appears throughout nature in surprising ways. The arrangement of leaves around a stem, the spiral pattern of seeds in a sunflower head, and the branching of trees all follow Fibonacci numbers. This pattern optimizes the plant's exposure to sunlight and rain."

     "Traditional Japanese pottery techniques have been refined over more than a thousand years. The wabi-sabi aesthetic embraces imperfection, finding beauty in irregular shapes, asymmetry, and the marks left by the firing process. Raku pottery involves removing pieces from the kiln while still glowing hot."

     "Continental drift theory, first proposed by Alfred Wegener in 1912, was initially rejected by most geologists. It was not until the discovery of seafloor spreading in the 1960s that the theory gained widespread acceptance, eventually evolving into the modern theory of plate tectonics."

     "The London Underground, opened in 1863, was the world's first underground railway. The Metropolitan Railway initially used steam locomotives, which filled the tunnels with smoke and soot. The system gradually converted to electric traction starting in 1890 with the City and South London Railway."

     "Bayesian statistics provides a framework for updating probability estimates as new evidence becomes available. Unlike frequentist approaches, Bayesian methods explicitly incorporate prior knowledge through prior probability distributions, which are updated via Bayes' theorem to produce posterior distributions."

     "The Great Wall of China is not a single continuous wall but rather a series of walls and fortifications built by various dynasties over more than two millennia. The most well-preserved sections date from the Ming Dynasty, which ruled from 1368 to 1644 and invested heavily in northern border defense."

     "Sourdough bread relies on a symbiotic culture of wild yeast and lactic acid bacteria rather than commercial yeast. The fermentation process typically takes twelve to twenty-four hours, producing a bread with a distinctive tangy flavor and chewy texture that commercial yeast breads cannot replicate."

     "The rings of Saturn are composed primarily of water ice particles ranging in size from tiny grains to chunks as large as houses. Despite their enormous extent spanning up to 282,000 kilometers in diameter the rings are remarkably thin, averaging only about 10 meters in thickness."

     "The development of the printing press by Johannes Gutenberg around 1440 revolutionized the spread of information in Europe. Before movable type, books were copied by hand, making them extremely expensive and rare. Within fifty years of Gutenberg's invention, an estimated twenty million volumes had been printed."

     "Coral reefs are sometimes called the rainforests of the sea due to their extraordinary biodiversity. Although they cover less than one percent of the ocean floor, coral reefs are home to approximately twenty-five percent of all marine species including fish, mollusks, and crustaceans."

     "The Voynich Manuscript, written in an unknown script and language, has puzzled cryptographers and linguists since its discovery in 1912. Despite extensive analysis by amateur and professional codebreakers including World War II veterans, no one has definitively deciphered the text or identified the language."

     "Machine learning algorithms can be broadly categorized into supervised learning, unsupervised learning, and reinforcement learning. Supervised learning uses labeled training data to learn a mapping from inputs to outputs, while unsupervised learning discovers hidden patterns in unlabeled data."

     "The human brain contains approximately 86 billion neurons, each forming thousands of synaptic connections. Neural plasticity allows the brain to reorganize itself by forming new connections throughout life, a process that underlies learning, memory formation, and recovery from brain injuries."

     "The ancient city of Petra in modern-day Jordan was carved directly into red sandstone cliffs by the Nabataean people around the 4th century BCE. The elaborate facades of temples and tombs remain remarkably well-preserved, showcasing a unique blend of Eastern and Hellenistic architectural traditions."

     "The theory of general relativity, published by Albert Einstein in 1915, describes gravity not as a force but as a curvature of spacetime caused by mass and energy. This revolutionary framework predicted phenomena such as gravitational lensing, time dilation, and the existence of black holes."))

(define *n-fillers* (vector-length *filler-paragraphs*))

;;; Seed PRNG for reproducible haystack layout.
(random-seed 42)

;;; ====
;;; Haystack Generation
;;; ====

;;; generate-haystack : Nat -> String -> (String . Nat)
;;; Build a haystack of approximately target-size characters with the
;;; needle inserted in the middle third. Returns (haystack . needle-index).
(define (generate-haystack target-size needle)
  ;; Accumulate filler paragraphs until we reach target size
  (let loop ([size 0] [paras '()] [idx 0])
    (if (>= size target-size)
        ;; Insert needle at a random position in the middle third
        (let* ([all-paras (reverse paras)]
               [n-paras (length all-paras)]
               [third (max 1 (quotient n-paras 3))]
               [insert-at (+ third (random third))]
               [result (insert-at-index all-paras insert-at needle)])
          (cons (join-paragraphs result) insert-at))
        ;; Add another filler
        (let ([para (vector-ref *filler-paragraphs* (modulo idx *n-fillers*))])
          (loop (+ size (string-length para) 2)
                (cons para paras)
                (+ idx 1))))))

(define (insert-at-index lst idx item)
  (let loop ([l lst] [i 0] [acc '()])
    (cond
      [(and (= i idx) (pair? l))
       (append (reverse acc) (list item) l)]
      [(null? l)
       (reverse (cons item acc))]
      [else
       (loop (cdr l) (+ i 1) (cons (car l) acc))])))

(define (join-paragraphs paragraphs)
  (let loop ([ps paragraphs] [acc ""])
    (if (null? ps)
        acc
        (loop (cdr ps)
              (string-append acc (car ps) "\n\n")))))

;;; ====
;;; S-NIAH System Prompt
;;; ====

(define *needle-phrase* "crystal-penguin-7429")

(define *needle-sentence*
  (string-append
    "IMPORTANT NOTICE: The secret passphrase for this document is: "
    *needle-phrase*
    ". Please remember this passphrase for verification purposes."))

(define *s-niah-system-prompt*
  (string-append
    "You are a Fold/Scheme agent. Your task is to find specific information "
    "hidden in a large text document.\n\n"
    "The text is stored in your environment under the key 'input'. "
    "It is too large to read all at once — use the environment tools to "
    "navigate it.\n\n"
    "Strategy:\n"
    "1. Peek at the input to understand its structure\n"
    "2. Use grep to search for relevant keywords\n"
    "3. Examine matching chunks carefully\n"
    "4. Report your finding with DONE(answer)\n\n"
    "Be systematic and concise. Do not guess — search the text.\n"))

;;; ====
;;; Runner
;;; ====

(define (run-s-niah target-size)
  (let* ([haystack-pair (generate-haystack target-size *needle-sentence*)]
         [haystack (car haystack-pair)]
         [needle-pos (cdr haystack-pair)]
         [provider (rlm-provider-vllm "Qwen/Qwen3-Next-80B-A3B-Instruct-FP8" 8000)]
         [config (make-rlm-config
                   provider
                   *s-niah-system-prompt*
                   10     ; max-steps
                   10000  ; max-fuel
                   2000   ; chunk-size
                   1      ; max-depth (no sub-agents needed for S-NIAH)
                   3)]    ; loop-window
         [task "Find the secret passphrase hidden in the text. Report only the passphrase value."])

    (display (format "=== S-NIAH Test ===\n"))
    (display (format "Haystack: ~a chars, ~a chunks\n"
                     (string-length haystack)
                     (+ 1 (quotient (string-length haystack) 2000))))
    (display (format "Needle at: paragraph ~a\n" needle-pos))
    (display (format "Expected: ~a\n\n" *needle-phrase*))
    (flush-output-port)

    (let ([result (rlm-run config task haystack)])
      (display (format "\n=== Result ===\n"))
      (display (format "Status: ~a\n" (rlm-run-result-status result)))
      (display (format "Output: ~a\n" (rlm-run-result-output result)))
      (display (format "Trajectory: ~a\n" (rlm-run-result-trajectory-hash result)))

      ;; Check answer
      (let ([output (format "~a" (rlm-run-result-output result))])
        (if (s-niah-contains? output *needle-phrase*)
            (begin
              (display "\n>>> PASS: Correct passphrase found!\n")
              #t)
            (begin
              (display (format "\n>>> FAIL: Expected '~a' in output\n" *needle-phrase*))
              #f))))))

(define (s-niah-contains? haystack needle)
  (let ([h-len (string-length haystack)]
        [n-len (string-length needle)])
    (if (> n-len h-len)
        #f
        (let loop ([i 0])
          (cond
            [(> (+ i n-len) h-len) #f]
            [(string=? (substring haystack i (+ i n-len)) needle) #t]
            [else (loop (+ i 1))])))))

;;; ====
;;; Run
;;; ====

;;; Run at multiple scales
(define (run-s-niah-suite sizes)
  (let loop ([remaining sizes] [results '()])
    (if (null? remaining)
        (begin
          (display "\n=== Summary ===\n")
          (for-each
            (lambda (r)
              (display (format "  ~aK chars: ~a\n"
                               (quotient (car r) 1000)
                               (if (cdr r) "PASS" "FAIL"))))
            (reverse results))
          results)
        (let ([pass? (run-s-niah (car remaining))])
          (display "\n")
          (loop (cdr remaining)
                (cons (cons (car remaining) pass?) results))))))

(run-s-niah-suite '(50000 200000 500000))
