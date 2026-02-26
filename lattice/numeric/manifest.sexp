(skill numeric
  (version "1.0.0")
  (path "lattice/numeric")
  (purity total)
  (stability stable)
  (fuel-bound (max (log-linear n) (quadratic n)))
  (deps (linalg algebra))

  (description
   "Foundation numerical transforms: complex number arithmetic, discrete Fourier
    transform (radix-2 FFT), windowing functions for spectral analysis, convolution
    and correlation, FFT-based convolution, and numeric polynomials (descending
    order, vector-based). These are the mathematical primitives that signal
    processing, autodiff, and statistics build upon.")

  (keywords (numerics fft dft complex-numbers convolution correlation
             polynomial window-functions hann hamming blackman kaiser))
  (aliases ())

  (concepts
    (concept numerical-computing
      (description "The domain of floating-point algorithms, approximation methods, and rigorous error analysis.")
      (parent mathematics)
      (synonyms numerics)))

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

   (convolution
    convolve-direct-full convolve-direct-same convolve-direct-valid
    convolve-fft convolve convolve-circular convolve-2d
    correlate-direct correlate autocorrelate
    matched-filter find-peaks vector-reverse normalize-signal))

  (modules
   (complex "complex.ss"
    "Complex number representation and arithmetic. Rectangular and polar forms.
     Transcendental functions: exp, log, sqrt, trig, hyperbolic.")
   (complex-bridge "complex-bridge.ss"
    "Bridge between complex number arithmetic and linalg vector/matrix operations.")
   (dft "dft.ss"
    "Discrete Fourier Transform: naive O(n^2) and radix-2 FFT O(n log n).
     Forward and inverse transforms. Spectrum analysis utilities.")
   (numeric/polynomial "polynomial.ss"
    "Numeric polynomials in descending coefficient order (vector-based).
     Evaluation, arithmetic, roots, and derivatives.")
   (window-functions "window-functions.ss"
    "Windowing functions for spectral analysis: Hann, Hamming, Blackman,
     Bartlett, Kaiser. Window energy and power normalization.")
   (convolution "convolution.ss"
    "Linear convolution: direct and FFT-based. Correlation and autocorrelation.
     2D convolution for image processing. Matched filtering.")
   (fft-convolve "fft-convolve.ss"
    "FFT-based convolution for large signals. Overlap-add method.")))
