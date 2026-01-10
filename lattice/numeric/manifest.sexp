(skill numeric
  (version "0.1.0")
  (tier 0)
  (path "lattice/numeric")
  (purity total)
  (stability stable)
  (fuel-bound "O(n log n) for FFT, O(n) for FIR filters, O(n*m) for 2D convolution")
  (deps ())

  (description
   "Numerical computing and signal processing library. Provides complex number
    arithmetic, discrete Fourier transform (naive and radix-2 FFT), digital
    filters (FIR, IIR, Butterworth, Chebyshev), convolution and correlation,
    wavelet transforms (Haar, Daubechies), spectral analysis (STFT, spectrogram,
    Welch PSD), and window functions.")

  (keywords (numerics signal-processing fft dft complex-numbers digital-filters
             wavelets convolution spectral-analysis iir fir butterworth))
  (aliases (signal dsp))

  (exports
   (complex
    make-complex complex? complex-real complex-imag real-num->complex i-value
    make-polar complex-magnitude complex-angle complex->polar polar->complex
    complex-add complex-sub complex-mul complex-div
    complex-negate complex-conjugate complex-reciprocal
    complex-equal? complex-zero? complex-real? complex-imaginary?
    complex-exp complex-log complex-pow complex-sqrt
    complex-sin complex-cos complex-tan complex-sinh complex-cosh complex-tanh
    complex->string complex-scale complex-dot complex-hermitian-product
    complex-zero complex-one complex-i complex-pi complex-e)

   (dft
    power-of-2? log2-int bit-reverse bit-reverse-copy
    dft-naive idft-naive
    fft-radix2-inplace! fft-radix2 ifft-radix2-inplace! ifft-radix2
    dft idft
    real->complex-vec complex->real-vec dft-real idft-real
    magnitude-spectrum phase-spectrum power-spectrum freq-bins
    zero-pad next-power-of-2 zero-pad-power-of-2)

   (window-functions
    rectangular-window hann-window hamming-window blackman-window
    bartlett-window kaiser-window bessel-i0
    apply-window apply-window-complex
    window-energy window-power make-window)

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

   (convolution
    convolve-direct-full convolve-direct-same convolve-direct-valid
    convolve-fft convolve convolve-circular convolve-2d
    correlate-direct correlate autocorrelate
    matched-filter find-peaks vector-reverse normalize-signal)

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
    spectral-centroid spectral-bandwidth spectral-rolloff))

  (modules
   (complex "complex.ss"
    "Complex number representation and arithmetic. Rectangular and polar forms.
     Transcendental functions: exp, log, sqrt, trig, hyperbolic.")
   (dft "dft.ss"
    "Discrete Fourier Transform: naive O(n^2) and radix-2 FFT O(n log n).
     Forward and inverse transforms. Spectrum analysis utilities.")
   (window-functions "window-functions.ss"
    "Windowing functions for spectral analysis: Hann, Hamming, Blackman,
     Bartlett, Kaiser. Window energy and power normalization.")
   (digital-filters "digital-filters.ss"
    "FIR and IIR digital filter design. Butterworth and Chebyshev lowpass.
     Biquad sections, filter cascades, frequency response analysis.")
   (convolution "convolution.ss"
    "Linear convolution: direct and FFT-based. Correlation and autocorrelation.
     2D convolution for image processing. Matched filtering.")
   (wavelet "wavelet.ss"
    "Discrete wavelet transform: Haar, Daubechies-4, Daubechies-6 families.
     Multi-level decomposition and reconstruction. Denoising via thresholding.")
   (spectral-analysis "spectral-analysis.ss"
    "Short-time Fourier transform and spectrogram computation. Welch power
     spectral density estimation. Spectral features: centroid, bandwidth, rolloff.")))
