;;; user/rlm/multi-hop.ss — Multi-Hop Needle-in-a-Haystack
;;;
;;; Extension of S-NIAH from "Recursive Language Models"
;;; (Zhang, Kraska, Khattab — arXiv:2512.24601).
;;;
;;; The answer requires following a chain of references across
;;; different parts of the text. Tests iterative search + reasoning.
;;;
;;; Run: RLM_INTEGRATION=1 scheme --script user/rlm/multi-hop.ss

(unless (getenv "RLM_INTEGRATION")
  (display "Skipping multi-hop test (set RLM_INTEGRATION=1 to enable)\n")
  (exit 0))

(load "boundary/pipeline/rlm-loop.ss")

;;; ====
;;; Filler Corpus (same as S-NIAH)
;;; ====

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
;;; Hop Chain Definitions
;;; ====
;;;
;;; Each chain is a list of (search-term . paragraph-text) pairs.
;;; The model must grep for search-term[0] (from the question), find the
;;; clue pointing to search-term[1], grep for that, and so on until
;;; it reaches the paragraph containing the final answer.

(define *answer* "midnight-falcon-3891")

(define (make-2hop-chain)
  (list
    (cons "Project Aurora"
          (string-append
            "The security access code for Project Aurora is maintained by the "
            "employee with identification number EMP-7291 in the Cryptographic "
            "Security Division. All inquiries about Project Aurora credentials "
            "must be directed to this employee through official channels. "
            "Unauthorized access attempts will be logged and reported."))
    (cons "EMP-7291"
          (string-append
            "Employee Record EMP-7291: Dr. Elena Vasquez, Cryptographic Security "
            "Division. Active credentials — Project Aurora access code: "
            "midnight-falcon-3891. Lab keycard: KY-2044. Building access: BA-1190. "
            "Last review: 2025-11-03. Status: Active. Next rotation: 2026-05-01."))))

(define (make-3hop-chain)
  (list
    (cons "Project Aurora"
          (string-append
            "Project Aurora credentials are managed under Security Protocol SP-2847. "
            "Contact the Protocol Administrator for current access codes. All "
            "credential rotation follows the standard quarterly schedule established "
            "by the Information Security Office."))
    (cons "SP-2847"
          (string-append
            "Security Protocol SP-2847: Access credential management for classified "
            "projects including Project Aurora. Responsibility delegated to employee "
            "EMP-7291 per directive dated 2025-08-15. All credential requests must "
            "reference this protocol number. Next scheduled review: 2026-02-01."))
    (cons "EMP-7291"
          (string-append
            "Employee Record EMP-7291: Dr. Elena Vasquez, Cryptographic Security "
            "Division. Active credentials — Project Aurora access code: "
            "midnight-falcon-3891. Lab keycard: KY-2044. Building access: BA-1190. "
            "Last review: 2025-11-03. Status: Active. Next rotation: 2026-05-01."))))

;;; ====
;;; Haystack Generation
;;; ====

;;; Build filler paragraphs to target size, then scatter-insert the clues
;;; at well-spaced positions.
(define (generate-multihop-haystack target-size chain)
  (let loop ([size 0] [paras '()] [idx 0])
    (if (>= size target-size)
        (let* ([all-paras (reverse paras)]
               [clue-texts (map cdr chain)]
               [with-clues (scatter-insert all-paras clue-texts)])
          (join-paragraphs with-clues))
        (let ([para (vector-ref *filler-paragraphs* (modulo idx *n-fillers*))])
          (loop (+ size (string-length para) 2)
                (cons para paras)
                (+ idx 1))))))

;;; scatter-insert : (List String) -> (List String) -> (List String)
;;; Insert items at well-spaced positions throughout the list.
;;; Divides the list into (k+1) zones and places one item per zone.
(define (scatter-insert paragraphs items)
  (let* ([n (length paragraphs)]
         [k (length items)]
         [zone (max 1 (quotient n (+ k 1)))])
    (let loop ([paras paragraphs]
               [remaining items]
               [idx 0]
               [next-insert (+ zone (random (max 1 (quotient zone 3))))]
               [acc '()])
      (cond
        ;; Done with paragraphs — append any leftover items
        [(null? paras)
         (reverse (append (reverse remaining) acc))]
        ;; Time to insert the next clue
        [(and (pair? remaining) (>= idx next-insert))
         (loop paras
               (cdr remaining)
               idx
               (+ idx zone (random (max 1 (quotient zone 3))))
               (cons (car remaining) acc))]
        ;; Regular paragraph
        [else
         (loop (cdr paras)
               remaining
               (+ idx 1)
               next-insert
               (cons (car paras) acc))]))))

(define (join-paragraphs paragraphs)
  (let loop ([ps paragraphs] [acc ""])
    (if (null? ps)
        acc
        (loop (cdr ps)
              (string-append acc (car ps) "\n\n")))))

;;; ====
;;; System Prompt
;;; ====

(define *multihop-system-prompt*
  (string-append
    "You are a Fold/Scheme agent. Your task is to find specific information "
    "in a large text document.\n\n"
    "The text is stored in your environment under the key 'input'. It is too "
    "large to read all at once — use the environment tools to navigate it.\n\n"
    "IMPORTANT: The answer may not be directly stated. You may need to follow "
    "references and cross-references to find it. Strategy:\n"
    "1. Search for keywords related to the question\n"
    "2. Read matching sections carefully for clues and references\n"
    "3. If a section refers to an identifier (employee ID, protocol number, "
    "reference code, etc.), search for that identifier too\n"
    "4. Store intermediate findings: (rlm-env-put! 'clue \"what you found\")\n"
    "5. Keep following the chain until you have the final answer\n"
    "6. Report with DONE(answer)\n\n"
    "Be systematic. Follow every lead. Do not guess.\n"))

;;; ====
;;; Runner
;;; ====

(define *task* "What is the access code for Project Aurora? Report only the code value.")

(define (run-multihop n-hops target-size)
  (let* ([chain (if (= n-hops 2) (make-2hop-chain) (make-3hop-chain))]
         [haystack (generate-multihop-haystack target-size chain)]
         [provider (rlm-provider-vllm "Qwen/Qwen3-Next-80B-A3B-Instruct-FP8" 8000)]
         [config (make-rlm-config
                   provider
                   *multihop-system-prompt*
                   15     ; max-steps (more room for multi-hop reasoning)
                   20000  ; max-fuel
                   2000   ; chunk-size
                   1      ; max-depth
                   3)]    ; loop-window
         )
    (display (format "=== Multi-Hop (~a hops) ===\n" n-hops))
    (display (format "Haystack: ~a chars, ~a chunks\n"
                     (string-length haystack)
                     (+ 1 (quotient (string-length haystack) 2000))))
    (display (format "Expected: ~a\n\n" *answer*))
    (flush-output-port)

    (let ([result (rlm-run config *task* haystack)])
      (display (format "\n=== Result ===\n"))
      (display (format "Status: ~a\n" (rlm-run-result-status result)))
      (display (format "Output: ~a\n" (rlm-run-result-output result)))
      (display (format "Trajectory: ~a\n" (rlm-run-result-trajectory-hash result)))

      ;; Check answer
      (let ([output (format "~a" (rlm-run-result-output result))])
        (if (string-contains-ci? output *answer*)
            (begin
              (display "\n>>> PASS: Correct access code found!\n")
              #t)
            (begin
              (display (format "\n>>> FAIL: Expected '~a' in output\n" *answer*))
              #f))))))

(define (string-contains-ci? haystack needle)
  (let ([h (string-downcase haystack)]
        [n (string-downcase needle)])
    (let ([h-len (string-length h)]
          [n-len (string-length n)])
      (if (> n-len h-len)
          #f
          (let loop ([i 0])
            (cond
              [(> (+ i n-len) h-len) #f]
              [(string=? (substring h i (+ i n-len)) n) #t]
              [else (loop (+ i 1))]))))))

(define (string-downcase s)
  (list->string (map char-downcase (string->list s))))

;;; ====
;;; Run
;;; ====

;; 2-hop at 100K, then 3-hop at 100K
(define (run-multihop-suite)
  (let* ([r1 (run-multihop 2 100000)]
         [_ (display "\n")]
         [r2 (run-multihop 3 100000)])
    (display "\n=== Summary ===\n")
    (display (format "  2-hop: ~a\n" (if r1 "PASS" "FAIL")))
    (display (format "  3-hop: ~a\n" (if r2 "PASS" "FAIL")))))

(run-multihop-suite)
