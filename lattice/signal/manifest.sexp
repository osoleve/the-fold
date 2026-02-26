(skill signal
  (version "0.1.0")
  (path "lattice/signal")
  (purity total)
  (stability stable)
  (fuel-bound (max (log-linear n) (quadratic n)))
  (deps (numeric algebra))

  (description
   "Digital signal processing: FIR/IIR filter design (Butterworth, Chebyshev),
    wavelet transforms (Haar, Daubechies), spectral analysis (STFT, spectrogram,
    Welch PSD), and polynomial algebra for filter stability analysis.")

  (keywords (signal-processing digital-filters fir iir butterworth chebyshev
             biquad wavelets haar daubechies dwt spectral-analysis stft
             spectrogram welch-psd periodogram filter-stability
             filter-cascade deconvolution))
  (aliases (dsp))

  (concepts
    (concept signal-processing
      (description "Digital processing of time-series signals: digital filters (FIR/IIR), wavelets, and spectral analysis.")
      (parent numerical-computing)
      (synonyms signal dsp digital-filter fir iir butterworth chebyshev biquad wavelet haar daubechies dwt stft spectrogram)))

  (exports
   (digital-filters
    sinc fir-lowpass fir-highpass fir-bandpass fir-bandstop fir-filter
    make-iir-filter iir-filter? iir-filter-b iir-filter-a iir-filter-signal
    make-biquad biquad? biquad-b0 biquad-b1 biquad-b2 biquad-a1 biquad-a2
    biquad-filter cascade-biquads
    butterworth-lowpass-poles bilinear-transform butterworth-lowpass
    chebyshev1-lowpass chebyshev1-poles
    poles-zeros->tf poly-from-roots poly-mul-binomial
    freqz eval-transfer-function eval-poly magnitude-response phase-response
    make-filter-state filter-state? filter-process-sample! filter-reset!
    dc-blocker one-pole-lowpass one-pole-highpass moving-average
    impulse-response step-response)

   (wavelet
    haar-scaling-filter haar-wavelet-filter
    daubechies-4-scaling-filter daubechies-4-wavelet-filter
    daubechies-6-scaling-filter daubechies-6-wavelet-filter
    qmf-wavelet-from-scaling reverse-filter
    convolve-downsample upsample-convolve
    dwt-step dwt idwt-step idwt
    get-wavelet-filters dwt-family idwt-family
    wavelet-decompose wavelet-reconstruct
    get-approximation get-details get-detail-level
    hard-threshold soft-threshold threshold-coefficients
    wavelet-denoise wavelet-energy wavelet-energy-distribution wavelet-energy-ratio)

   (spectral-analysis
    stft-frame stft istft
    spectrogram power-spectrogram log-spectrogram
    periodogram welch-psd
    spectrogram-frequencies spectrogram-times
    spectral-centroid spectral-bandwidth spectral-rolloff)

   (signal-poly
    filter-num-poly filter-den-poly filter-coprime? filter-simplify
    filter-stable? filter-cascade filter-parallel
    deconvolve deconvolve-exact?
    numeric->signal-poly signal->numeric-poly
    sig-poly-degree sig-poly-coeffs sig-poly-gcd
    sig-poly->string filter->string))

  (modules
   (digital-filters "digital-filters.ss"
    "FIR and IIR digital filter design. Butterworth and Chebyshev lowpass.
     Biquad sections, filter cascades, frequency response analysis.")
   (wavelet "wavelet.ss"
    "Discrete wavelet transform: Haar, Daubechies-4, Daubechies-6 families.
     Multi-level decomposition and reconstruction. Denoising via thresholding.")
   (spectral-analysis "spectral-analysis.ss"
    "Short-time Fourier transform and spectrogram computation. Welch power
     spectral density estimation. Spectral features: centroid, bandwidth, rolloff.")
   (signal-poly "signal-poly.ss"
    "Polynomial algebra integration for signal processing. Filter stability analysis
     via Jury criterion. Filter simplification, cascade/parallel combination,
     deconvolution as polynomial division.")))
