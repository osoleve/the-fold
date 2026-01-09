(skill info
  (version "0.1.0")
  (tier 1)
  (path "lattice/info")
  (purity total)
  (stability stable)
  (fuel-bound "O(n) for entropy/divergence, O(n log n) for Huffman, O(n^2) for Blahut-Arimoto")
  (deps ())

  (description
   "Information theory library implementing Shannon entropy, divergence measures,
    channel capacity computation, rate-distortion theory, and coding algorithms.
    Covers entropy variants (Renyi, min, collision), statistical divergences
    (KL, JS, Hellinger, chi-squared), channel models (BSC, BEC, AWGN), and
    source/channel coding (Huffman, arithmetic, LZ78, Hamming).")

  (keywords (information-theory entropy divergence channel-capacity rate-distortion
             huffman-coding arithmetic-coding compression error-correction shannon))
  (aliases (information entropy coding))

  (exports
   (entropy
    entropy entropy-normalized max-entropy entropy-ratio binary-entropy
    joint-entropy conditional-entropy conditional-entropy-direct
    mutual-information mutual-information-from-joint
    cross-entropy kl-divergence symmetric-kl jensen-shannon-divergence
    gaussian-entropy uniform-entropy exponential-entropy laplace-entropy
    sample-entropy count-occurrences
    variation-of-information normalized-mutual-information
    renyi-entropy collision-entropy min-entropy hartley-entropy
    surprisal pointwise-mutual-information normalized-pmi entropy-power)

   (statistical-measures
    bhattacharyya-coefficient bhattacharyya-distance
    hellinger-distance hellinger-distance-from-bc
    total-variation-distance chi-squared-divergence symmetric-chi-squared
    jeffreys-divergence alpha-divergence squared-hellinger-distance
    triangular-discrimination pearson-divergence neyman-divergence
    k-divergence squared-loss euclidean-distance matusita-distance fidelity
    all-divergences valid-distribution? normalize-distribution)

   (channel-capacity
    bsc-capacity bsc-transition-matrix bsc-mutual-information
    bec-capacity z-channel-capacity z-channel-transition-matrix
    awgn-capacity awgn-capacity-db awgn-capacity-bandwidth snr-for-capacity
    dmc-mutual-information dmc-output-distribution dmc-joint-distribution
    dmc-capacity-symmetric make-uniform-dist
    blahut-arimoto ba-iterate ba-compute-phi ba-update-dist
    dmc-capacity-upper-bound dmc-capacity-lower-bound
    qary-symmetric-capacity parallel-channel-capacity cascaded-channel-capacity-bound
    achievable-rate? max-reliable-rate spectral-efficiency
    bsc-random-coding-exponent awgn-sphere-packing-exponent
    db-to-linear linear-to-db channel-summary)

   (rate-distortion
    squared-error absolute-error hamming-distortion
    mse mae rmse normalized-mse snr snr-db psnr mean variance
    gaussian-rate-distortion gaussian-distortion-rate
    binary-rate-distortion binary-distortion-rate binary-entropy-inverse
    uniform-quantize uniform-quantize-list uniform-quantization-distortion
    quantization-bits lloyd-max-quantizer lloyd-max-iterate
    compute-boundaries compute-centroids partition-by-boundaries
    lloyd-max-quantize find-region-level
    vq-codebook-distance vq-find-nearest vq-quantize
    operational-rate-distortion unique-values rd-gap
    quantizer-entropy count-values entropy-coded-rate
    dither-quantize high-rate-bits-per-sample high-rate-distortion rd-summary)

   (coding
    make-huffman-leaf make-huffman-internal huffman-leaf? huffman-symbol
    huffman-children huffman-prob build-huffman-tree huffman-codes
    huffman-encode huffman-decode huffman-average-length huffman-efficiency
    arithmetic-encode arithmetic-decode arithmetic-code-length
    build-cumulative-probs find-symbol-for-code
    lz78-encode lz78-decode lz78-find-longest-match
    rle-encode rle-decode
    parity-encode parity-check parity-decode
    repetition-encode repetition-decode majority-vote
    hamming74-encode hamming74-decode hamming74-syndrome
    hamming-distance minimum-distance code-rate
    error-correcting-capability error-detecting-capability
    compression-ratio space-savings)

   (epiplexity
    prequential-epiplexity prequential-epiplexity-scalar requential-epiplexity))

  (modules
   (entropy "entropy.ss"
    "Shannon entropy and variants: joint, conditional, mutual information.
     Includes Renyi, min-entropy, collision entropy. Sample entropy estimation.")
   (statistical-measures "statistical-measures.ss"
    "Probability distribution divergence measures: KL, JS, Hellinger, Bhattacharyya,
     chi-squared, total variation, alpha-divergence, and many more.")
   (channel-capacity "channel-capacity.ss"
    "Communication channel capacity computation. BSC, BEC, Z-channel, AWGN models.
     Blahut-Arimoto algorithm for general DMC capacity optimization.")
   (rate-distortion "rate-distortion.ss"
    "Rate-distortion theory: distortion metrics, R(D) curves for Gaussian/binary
     sources, Lloyd-Max quantization, vector quantization, entropy-coded rates.")
   (coding "coding.ss"
    "Source and channel coding: Huffman trees, arithmetic coding, LZ78 dictionary
     compression, run-length encoding, parity checks, Hamming (7,4) codes.")
   (epiplexity "epiplexity.ss"
    "Prequential and requential epiplexity measures for sequence prediction
     evaluation and model comparison.")))
