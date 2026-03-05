;;; skills/signal.sexp — Skill curation file

(skill signal
  (description
    "Digital signal processing: FIR/IIR filter design (Butterworth, Chebyshev),\n    wavelet transforms (Haar, Daubechies), spectral analysis (STFT, spectrogram,\n    Welch PSD), and polynomial algebra for filter stability analysis.")

  (keywords (signal-processing digital-filters fir iir butterworth chebyshev biquad wavelets haar daubechies dwt spectral-analysis stft spectrogram welch-psd periodogram filter-stability filter-cascade deconvolution))

  (aliases (dsp))

  (concepts (signal-processing))

  (modules
    digital-filters
    wavelet
    spectral-analysis
    signal-poly
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
