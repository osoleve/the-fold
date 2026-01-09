((name "numeric")
 (purpose "Numerical computing")
 (description "Numerical algorithms including complex numbers, FFT/DFT,
convolution, and transcendental functions. Provides the mathematical
foundation for signal processing and scientific computing.")
 (modules
  ((complex.ss "Complex number arithmetic - 56 tests")
   (dft.ss "FFT and DFT algorithms - 46 tests")
   (convolution.ss "Convolution operations")))
 (dependencies (base linalg))
 (note "Transcendental functions are in fp/numeric/transcendental.ss"))
