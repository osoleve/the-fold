((name "numeric")
 (purpose "Numerical computing")
 (description "Numerical algorithms including complex numbers, FFT/DFT,
convolution, and transcendental functions. Provides the mathematical
foundation for signal processing and scientific computing.")
 (modules
  ((complex.ss "Complex number arithmetic - 56 tests")
   (dft.ss "FFT and DFT algorithms - 46 tests")
   (convolution.ss "Convolution operations")
   (polynomial.ss "Numeric polynomials - descending order, vector-based")
   (digital-filters.ss "IIR/FIR digital filters, Butterworth, Chebyshev")
   (window-functions.ss "Hann, Hamming, Blackman, Kaiser windows")
   (wavelet.ss "Haar, Daubechies wavelet transforms")
   (spectral-analysis.ss "STFT, spectrogram, Welch PSD")
   (signal-poly.ss "Filter polynomial algebra - stability, simplification, cascade")))
 (dependencies (base linalg))
 (notes
  ("Transcendental functions are in fp/numeric/transcendental.ss"
   "For polynomial GCD/division, use algebra/poly-bridge.ss alongside polynomial.ss"
   "Load polynomial.ss BEFORE poly-bridge.ss to avoid name collisions")))
